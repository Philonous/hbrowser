-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- License: MIT
--

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, StandaloneDeriving, ScopedTypeVariables, NamedFieldPuns #-}

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk (set, AttrOp((:=)), on)
import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

import Data.Bits
import Data.List
import Data.Maybe

import Data.List.PointedList.Circular as PL

import Data.IORef

import qualified Data.Map as M
import System.Glib.Flags
import System.IO

import qualified Web.HBrowser.WebViewInternals as Web
-- import Web.HBrowser.SetProxy (setProxy) -- TODO: uncomment]
import Web.HBrowser.ViewContainer
import Web.HBrowser.WebMonad as WM
import Web.HBrowser.Scripting

-- Design: The keymap IORef holds a stack (aka list) of keymaps, 
-- a #submap# pushes one keymap on the top, so we can exit it by poping the map
-- INVARIANT: the keymap stack should never be empty

deleteTab = lift $ do
  web <- ask
  current <- currentView
  deleted <- liftM PL.delete . liftIO $ readIORef (tabs web)
  case deleted of 
      Nothing -> openURL "about:blank"
      Just x -> liftIO $ do
        GTK.widgetDestroy current
        writeIORef (tabs web) x
        return ()
  updateView

newTab :: String -> WebMonad ()
newTab url = do
  web <- ask
  webView <- createView
  liftIO $ Web.webViewLoadUri webView url
  modifyTabs $ insertRight webView
  updateView

keepKeymap :: (MonadState NextMap m) => m ()
keepKeymap = put Keep

-- modifier flags 
shift        = fromFlags [ GTK.Shift        ]
lock         = fromFlags [ GTK.Lock         ]
control      = fromFlags [ GTK.Control      ]
alt          = fromFlags [ GTK.Alt          ]
alt2         = fromFlags [ GTK.Alt2         ]
alt3         = fromFlags [ GTK.Alt3         ]
alt4         = fromFlags [ GTK.Alt4         ]
alt5         = fromFlags [ GTK.Alt5         ]
button1      = fromFlags [ GTK.Button1      ]
button2      = fromFlags [ GTK.Button2      ]
button3      = fromFlags [ GTK.Button3      ]
button4      = fromFlags [ GTK.Button4      ]
button5      = fromFlags [ GTK.Button5      ]
super        = fromFlags [ GTK.Super        ]
hyper        = fromFlags [ GTK.Hyper        ]
meta         = fromFlags [ GTK.Meta         ]
release      = fromFlags [ GTK.Release      ]
modifierMask = fromFlags [ GTK.ModifierMask ]

-- push one map on the stack
push :: a -> [a] -> [a]
push x xs = x:xs

-- remove the top element unless it's the last
pop :: [t] -> [t]
pop (x:y:xs)  = y:xs
pop xs        = xs

-- look at the top element
top :: [t] -> t
top (x:xs)   = x
top _        = error "top on empty stack"

io = liftIO

submap :: Keymap -> WebInputMonad ()
submap map = do
  ref <- asks keymapRef
  liftIO $ modifyIORef ref (push map)
  put Keep
  
setKeymap :: (MonadReader Web m, MonadIO m) => Keymap -> m ()
setKeymap map =  ask >>= \(keymapRef -> ref) -> io $ writeIORef ref [map]

modifyKeymap :: ([Keymap] -> [Keymap]) -> WebMonad ()
modifyKeymap f =  ask >>= \(keymapRef -> ref) -> io $ 
                    readIORef ref >>= writeIORef ref . f
  
loadURL :: String -> WebInputMonad ()
loadURL url = do
  page <- currentView
  io $ Web.webViewLoadUri page url

myKeymap  :: ( GTK.EditableClass self, GTK.EntryClass self) => self -> Keymap
myKeymap bar = M.fromList
           [ ((0, GTK.keyFromName "l"), submap loadKeymap)
           , ((0, GTK.keyFromName "h"), io $ putStrLn "hello world" )
           , ((0, GTK.keyFromName "o"), withInput "open: " "http://" openURL  )
           , ((0, GTK.keyFromName "t"), withInput "newtab: " "http://" newTab )
           , ((0, GTK.keyFromName "d"), deleteTab )
           , ((0, GTK.keyFromName "e"), nextTab )
           , ((0, GTK.keyFromName "w"), prevTab )
           , ((0, GTK.keyFromName "f"), histForward )
           , ((0, GTK.keyFromName "b"), histBack    )
           , ((0, GTK.keyFromName "q"), runScriptFromFile "follow-selected")
           , ((0, GTK.keyFromName "f"), runScriptFromFile "follow-numbers-new-tab")
           , ((0, GTK.keyFromName "r"), withCurrentViewIO Web.webViewReloadBypassCache)
           ]

loadKeymap :: Keymap
loadKeymap = M.fromList
             [ ((0, GTK.keyFromName "r"), loadURL "http://reddit.com")
             , ((0, GTK.keyFromName "t"), loadURL "http://tagesschau.de")
             , ((0, GTK.keyFromName "g"), loadURL "http://google.com")
             ]


myMousemap :: Mousemap
myMousemap = M.empty

activateInput :: String -> WebMonad ()
activateInput prefill= do
  bar <- asks inputEntry
  container <- asks inputBox
  liftIO $ do 
    GTK.widgetShow container
  -- grabbing focus seems to select the whole content of the entry
  -- so we do it first and remove the selection afterwards
    GTK.widgetGrabFocus bar
    set bar [ GTK.entryText := prefill 
            , GTK.editablePosition := (-1)
            ]

  
histBack = lift . withCurrentView $ liftIO . Web.webViewGoBack
histForward = lift . withCurrentView $ liftIO . Web.webViewGoForward

deactivateInput = do
  widget <- asks inputBox
  view <- currentView
  actionRef <- asks inputAction
  liftIO $ do
    GTK.widgetHide widget
    GTK.widgetGrabFocus view
    writeIORef actionRef (const $ return ())

handleKey :: Web -> GTK.EventM GTK.EKey Bool
handleKey web = do 
  keyVal <-  GTK.eventKeyVal
  mods <- fromFlags <$> GTK.eventModifier
  keymap <- io $ top <$> readIORef (keymapRef web)
  case M.lookup (mods, keyVal) (map `M.union` keymap) of 
    Nothing -> return False
    Just action -> do
      (_, nextmap) <- io $ runReaderT (runStateT action Reset) web
      case nextmap of
        Reset -> io $ writeIORef (keymapRef web) [keymap]
        Back  -> io $ modifyIORef (keymapRef web) pop
        Keep  -> return ()
      return True
  where
    map = M.fromList [ ((control,GTK.keyFromName "g") , resetMap) ]
    resetMap = setKeymap (keymap $ config web)

handleEntryKey  :: (GTK.WidgetClass self) =>
     Web -> self -> GTK.EventM GTK.EKey Bool
handleEntryKey web bar = do
  view <- io $ runReaderT currentView web
  keyName <- GTK.eventKeyName
  mods <- fromFlags <$> GTK.eventModifier
  case (mods, keyName) of
    (0, "Escape") -> io $ runReaderT deactivateInput web
    _                         -> return ()
  return False

handleMouse :: Web -> GTK.EventM GTK.EButton Bool
handleMouse web = do
  button <- GTK.eventButton
  liftIO . putStrLn $ case button of 
    GTK.LeftButton   -> "left"
    GTK.MiddleButton -> "middle"
    GTK.RightButton  -> "right"
    GTK.OtherButton x -> show x
  return True

(<$$>) = fmap.fmap

deriving instance Show Web.LoadStatus
   
defaultRenderStatus :: WebMonad [Char]
defaultRenderStatus = do 
  conf <- ask
  webView <- currentView
  liftIO $ do 
    loadStatus   <- GTK.get webView Web.webViewLoadStatus
    loadProgress <- GTK.get webView Web.webViewProgress
    uri <- fromMaybe "" <$> GTK.get webView Web.webViewUri
    (hoveringTitle', hoveringURL') <- readIORef (WM.hovering conf)
    let hoveringURL = maybe "" (\x -> "(" ++ x ++ ")") hoveringURL'
    let status = case loadStatus of 
          Web.LoadFinished -> "done"
          Web.LoadFailed  -> "failed"
          _               -> show (floor $ loadProgress *100) ++ "%"
    title        <- GTK.get webView Web.webViewTitle
    return $ concat [ "[", status, "]"
                    , hoveringURL
                    , fromMaybe "" title , " "
                    , uri]

withInput :: String -> String -> (String -> WebMonad ()) -> WebInputMonad ()
withInput labelText prefill action = lift $ do
  label <- asks inputLabel
  actionRef <- asks inputAction
  liftIO $ writeIORef actionRef action
  liftIO $ GTK.labelSetText label labelText
  activateInput prefill

openURL :: String -> WebMonad ()
openURL url = do
  view <- currentView
  liftIO $ Web.webViewLoadUri view url

updateTabBar :: WebMonad ()
updateTabBar = do 
  tabRef <- asks tabs 
  tabs <- liftIO $ readIORef tabRef
  showTabs <- asks $ showTabs . config
  liftIO . showTabs $ toList tabs

main = do
  GTK.initGUI
  
--  setProxy "http://localhost:8118/" -- TODO: uncomment
  
  window <- GTK.windowNew
  
  box <- GTK.vBoxNew False 0
  
  inputLabel <- GTK.labelNew Nothing
  inputAction <- newIORef (const $ return ())
  inputEntry <- GTK.entryNew
  GTK.entrySetHasFrame inputEntry False
  
  inputBox <- GTK.hBoxNew False 0
  GTK.boxPackStart inputBox inputLabel GTK.PackNatural 0
  GTK.boxPackStart inputBox inputEntry GTK.PackGrow 0

  tabLabel <- GTK.labelNew $ Nothing
  (tabs, initialView) <- containerNew
  
  hovering <- newIORef (Nothing, Nothing)
  
  scrollWindow <- GTK.scrolledWindowNew Nothing Nothing
  
  GTK.containerAdd scrollWindow initialView
  
  statusbar <- GTK.labelNew Nothing
  GTK.set statusbar [ GTK.miscXalign  GTK.:= 0 ]
  
  GTK.containerAdd window box
  
  GTK.boxPackStart box tabLabel     GTK.PackNatural 0
  GTK.boxPackStart box scrollWindow GTK.PackGrow    0
  GTK.boxPackStart box statusbar    GTK.PackNatural 0
  GTK.boxPackStart box inputBox     GTK.PackNatural 0  
  
  let showStatus msg = GTK.labelSetText statusbar msg >> return ()
  let keymap = myKeymap inputEntry 
  let showTabs = GTK.labelSetText tabLabel . show <=< titleList
      
  keymapRef <- newIORef [keymap]
  mousemapRef <- newIORef [myMousemap]
  
  let config = WebConf{ keymap 
                      , mousemap = myMousemap
                      , showTabs
                      , showStatus
                      , renderStatus = defaultRenderStatus
                      , homeURL = "http://ixquick.com"
                      , jsScriptDir = "/home/uart14/.hbrowser/scripts"
                      }
  let web = Web     { keymapRef
                    , mousemapRef 
                    , tabs
                    , container = scrollWindow
                    , hovering
                    , config
                    , inputAction
                    , inputEntry
                    , inputLabel
                    , inputBox
                    }
  let runWebMonad action = runReaderT action web
  
  runWebMonad $ setupView initialView
  
  GTK.on scrollWindow GTK.keyPressEvent $ handleKey web
  GTK.on inputEntry GTK.keyPressEvent $ handleEntryKey web inputEntry
  
  GTK.on scrollWindow GTK.buttonPressEvent $ handleMouse web
    
  GTK.on inputEntry GTK.entryActivate $ do
    input <- GTK.entryGetText inputEntry
    action <- readIORef inputAction
    runWebMonad $ action input >> deactivateInput >> updateBars
    
  GTK.widgetShowAll window
--   GTK.widgetHide inputEntry
  GTK.onDestroy window GTK.mainQuit
  runWebMonad $ deactivateInput >> updateView
  GTK.mainGUI
  