-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- License: MIT
--

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, StandaloneDeriving, ScopedTypeVariables #-}

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

import qualified Web.HBrowser.WebViewInternals as Web
-- import Web.HBrowser.SetProxy (setProxy) -- TODO: uncomment]
import Web.HBrowser.ViewContainer
import Web.HBrowser.WebMonad as WM

-- Design: The keymap IORef holds a stack (aka list) of keymaps, 
-- a #submap# pushes one keymap on the top, so we can exit it by poping the map
-- INVARIANT: the keymap stack should never be empty

deleteTab = do
  web <- ask
  current <- currentView
  liftIO $ do 
    deleted <- PL.delete `fmap` readIORef (tabs web)
    case deleted of 
      Nothing -> Web.webViewLoadUri current "about:blank"
      Just x -> do
        GTK.widgetDestroy current
        writeIORef (tabs web) x
        return ()
  lift updateView

newTab :: String -> WebInputMonad ()
newTab url = lift $ do
  web <- ask
  webView <- createView
  liftIO $ Web.webViewLoadUri webView url
  withTabs $ insertRight webView
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
  ref <- asks mapRef
  liftIO $ modifyIORef ref (push map)
  put Keep
  
setKeymap :: (MonadReader Web m, MonadIO m) => Keymap -> m ()
setKeymap map =  ask >>= \(mapRef -> ref) -> io $ writeIORef ref [map]

modifyKeymap :: ([Keymap] -> [Keymap]) -> WebMonad ()
modifyKeymap f =  ask >>= \(mapRef -> ref) -> io $ 
                    readIORef ref >>= writeIORef ref . f
  
loadURL :: String -> WebInputMonad ()
loadURL url = do
  page <- currentView
  io $ Web.webViewLoadUri page url

myKeymap  :: ( GTK.EditableClass self, GTK.EntryClass self) => self -> Keymap
myKeymap bar = M.fromList
           [ ((0, GTK.keyFromName "l"), submap loadKeymap)
           , ((0, GTK.keyFromName "h"), io $ putStrLn "hello world" )
           , ((0, GTK.keyFromName "o"), io $ activateInputBar bar "http://"  )
           , ((0, GTK.keyFromName "n"), newTab "http://tagesschau.de"  )
           , ((0, GTK.keyFromName "x"), deleteTab )
           , ((0, GTK.keyFromName "e"), nextTab >> liftIO (putStrLn "next"))
           , ((0, GTK.keyFromName "w"), prevTab >> liftIO (putStrLn "prev"))
           ]

loadKeymap :: Keymap
loadKeymap = M.fromList
             [ ((0, GTK.keyFromName "r"), loadURL "http://reddit.com")
             , ((0, GTK.keyFromName "t"), loadURL "http://tagesschau.de")
             , ((0, GTK.keyFromName "g"), loadURL "http://google.com")
             ]


activateInputBar bar prefill= do
  GTK.widgetShow bar
  -- grabbing focus seems to select the whole content of the entry
  -- so we do it first and remove the selection later
  GTK.widgetGrabFocus bar  
  set bar [ GTK.entryText := prefill 
          , GTK.editablePosition := (-1)
          ]


deactivateInputBar bar view = do
  GTK.widgetHide bar
  GTK.widgetGrabFocus view


handleKey :: Web -> GTK.EventM GTK.EKey Bool
handleKey web = do 
  keyVal <-  GTK.eventKeyVal
  mods <- fromFlags <$> GTK.eventModifier
  keymap <- io $ top <$> readIORef (mapRef web)
  case M.lookup (mods, keyVal) (map `M.union` keymap) of 
    Nothing -> return False
    Just action -> do
      (_, nextmap) <- io $ runReaderT (runStateT action Reset) web
      case nextmap of
        Reset -> io $ writeIORef (mapRef web) [keymap]
        Back  -> io $ modifyIORef (mapRef web) pop
        Keep  -> return ()
      return True
  where
    map = M.fromList [ ((0,GTK.keyFromName "q") , resetMap) ]
    resetMap = setKeymap (keymap $ config web)

handleEntryKey  :: (GTK.WidgetClass self) =>
     Web -> self -> GTK.EventM GTK.EKey Bool
handleEntryKey conf bar = do
  view <- io $ runReaderT currentView conf
  keyName <- GTK.eventKeyName
  mods <- fromFlags <$> GTK.eventModifier
  case (mods, keyName) of
    (0, "Escape") -> io $ deactivateInputBar bar view
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


main = do
  GTK.initGUI
  
--  setProxy "http://localhost:8118/" -- TODO: uncomment
  
  window <- GTK.windowNew
  
  box <- GTK.vBoxNew False 0
  
  urlBar <- GTK.entryNew
  GTK.entrySetHasFrame urlBar False
  
  tabLabel <- GTK.labelNew $ Nothing
  (tabs, initialView) <- containerNew
  
  hovering <- newIORef (Nothing, Nothing)
  
  scrollWindow <- GTK.scrolledWindowNew Nothing Nothing
  
  statusbar <- GTK.labelNew Nothing
  GTK.set statusbar [ GTK.miscXalign  GTK.:= 0 ]
  
  GTK.containerAdd window box
  
  GTK.boxPackStart box tabLabel     GTK.PackNatural 0
  GTK.boxPackStart box scrollWindow GTK.PackGrow    0
  GTK.boxPackStart box statusbar    GTK.PackNatural 0
  GTK.boxPackStart box urlBar       GTK.PackNatural 0  
  
  let showStatus msg = GTK.labelSetText statusbar msg >> return ()
  let keymap = myKeymap urlBar 
  let showTabs = GTK.labelSetText tabLabel . show <=< titleList
      
  mapRef <- newIORef [keymap]
  
  let conf = WebConf  keymap showTabs showStatus defaultRenderStatus "http://ixquick.com"
  let web = Web mapRef tabs scrollWindow hovering conf
  
  runReaderT (setupView initialView) web
  
  GTK.on scrollWindow GTK.keyPressEvent $ handleKey web
  GTK.on urlBar GTK.keyPressEvent $ handleEntryKey web urlBar
    
  GTK.on urlBar GTK.entryActivate $ do
    url <- GTK.entryGetText urlBar
    cur <- currentView' tabs
    Web.webViewLoadUri cur url
    deactivateInputBar urlBar scrollWindow  
  GTK.widgetShowAll window
--   GTK.widgetHide urlBar
  GTK.onDestroy window GTK.mainQuit
  deactivateInputBar urlBar scrollWindow
  GTK.mainGUI
