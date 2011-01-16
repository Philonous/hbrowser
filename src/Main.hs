-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- LICENSE: MIT; see LICENSE file
--
-- MODULE: Main
--
-- DESCRIPTION: scriptable webkit based browser
--

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, StandaloneDeriving, ScopedTypeVariables, NamedFieldPuns #-}

module Main where 

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
  tab <- createTab
  liftIO $ Web.webViewLoadUri (tabView tab) url
  modifyTabs $ insertRight tab
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
  
myKeymap  :: ( GTK.EditableClass self, GTK.EntryClass self) => self -> Keymap
myKeymap bar = M.fromList
           [ ((0, GTK.keyFromName "l"), submap loadKeymap)
           , ((0, GTK.keyFromName "h"), io $ putStrLn "hello world" )
           , ((0, GTK.keyFromName "o"), withInput "open: " "http://" openURL  )
           , ((control, GTK.keyFromName "o"), currentUrl >>= \url -> 
               withInput "open: " (fromMaybe "http://" url) openURL  )
           , ((0, GTK.keyFromName "t"), withInput "newtab: " "http://" newTab )
           , ((0, GTK.keyFromName "d"), deleteTab )
           , ((0, GTK.keyFromName "e"), nextTab )
           , ((0, GTK.keyFromName "w"), prevTab )
           , ((0, GTK.keyFromName "f"), histForward )
           , ((0, GTK.keyFromName "b"), histBack    )
           , ((0, GTK.keyFromName "q"), runScriptFromFile "follow-selected")
           , ((0, GTK.keyFromName "f"), runScriptFromFile "follow-numbers-new-tab")
           , ((0, GTK.keyFromName "r"), withCurrentViewIO Web.webViewReloadBypassCache)
           , ((0, GTK.keyFromName "v"), typeThrough )      
           ]

loadKeymap :: Keymap
loadKeymap = M.fromList
             [ ((0, GTK.keyFromName "r"), openURL "http://reddit.com")
             , ((0, GTK.keyFromName "t"), openURL "http://tagesschau.de")
             , ((0, GTK.keyFromName "g"), openURL "http://google.com")
             ]


myMousemap :: Mousemap
myMousemap = [ ((0, GTK.LeftButton), hoveringLink >>= maybe (return ()) openURL)
             , ((0, GTK.MiddleButton), hoveringLink >>= maybe (return ()) (lift . newTab))
             ]

activateInput :: (MonadWeb m) => String -> m ()
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

typeThrough :: WebInputMonad ()
typeThrough = do 
  through <- asks typeThroughRef
  io $ writeIORef through True

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

withInput :: (MonadWeb m) =>String -> String -> (String -> WebMonad ()) -> m ()
withInput labelText prefill action =  do
  label <- asks inputLabel
  actionRef <- asks inputAction
  liftIO $ writeIORef actionRef action
  liftIO $ GTK.labelSetText label labelText
  activateInput prefill

openURL :: (MonadWeb m) => String -> m ()
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
  (tabs, initialTab) <- containerNew
  
  hovering <- newIORef (Nothing, Nothing)
  
  eventBox <- GTK.eventBoxNew
  
  GTK.containerAdd eventBox (tabScrolledWindow initialTab)
  
  statusbar <- GTK.labelNew Nothing
  GTK.set statusbar [ GTK.miscXalign  GTK.:= 0 ]
  
  GTK.containerAdd window box
  
  GTK.boxPackStart box tabLabel  GTK.PackNatural 0
  GTK.boxPackStart box eventBox  GTK.PackGrow    0
  GTK.boxPackStart box statusbar GTK.PackNatural 0
  GTK.boxPackStart box inputBox  GTK.PackNatural 0  
  
  let showStatus msg = GTK.labelSetText statusbar msg >> return ()
  let keymap = myKeymap inputEntry 
  let showTabs = GTK.labelSetText tabLabel . show <=< titleList
      
  keymapRef <- newIORef [keymap]
  mousemapRef <- newIORef [myMousemap]
  typeThroughRef <- newIORef False
  
  let config = WebConf{ keymap 
                      , mousemap = myMousemap
                      , showTabs
                      , showStatus
                      , renderStatus = defaultRenderStatus
                      , homeURL = "http://ixquick.com"
                      , jsScriptDir = "/home/uart14/.hbrowser/scripts"
                      , typeThroughOnMissmatch = False
                      , mouseThroughOnMissmatch = True
                      }
  let web = Web     { keymapRef
                    , mousemapRef 
                    , tabs
                    , container = eventBox
                    , hovering
                    , config
                    , inputAction
                    , inputEntry
                    , inputLabel
                    , inputBox
                    , typeThroughRef
                    }
  let runWebMonad action = runReaderT action web
  
  runWebMonad . setupView $ tabView initialTab
  
  GTK.on inputEntry GTK.keyPressEvent $ handleEntryKey web inputEntry
  
  GTK.on inputEntry GTK.entryActivate $ do
    input <- GTK.entryGetText inputEntry
    action <- readIORef inputAction
    runWebMonad $ action input >> deactivateInput >> updateBars
    
  GTK.widgetShowAll window
--   GTK.widgetHide inputEntry
  GTK.onDestroy window GTK.mainQuit
  runWebMonad $ deactivateInput >> updateView
  GTK.mainGUI
  