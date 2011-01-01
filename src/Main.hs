-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- License: MIT
--

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, StandaloneDeriving #-}

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

type Keymap = M.Map KeyDef (WebInputMonad ())
type ModifierFlags = Int 
type KeyDef = (ModifierFlags, GTK.KeyVal)
type WebMonad = ReaderT WebConf IO
type MapRef = IORef [Keymap]

-- Design: The keymap IORef holds a stack (aka list) of keymaps, 
-- a #submap# pushes one keymap on the top, so we can exit it by poping the map
-- INVARIANT: the keymap stack should never be empty


data WebConf = WebConf 
  { mapRef     :: MapRef
  , tabs       :: Tabs
  , container  :: GTK.ScrolledWindow
  , defaultMap :: Keymap 
  , showTabs   :: [Web.WebView] -> IO ()
  } 

data NextMap = Reset | Keep | Back 
type WebInputMonad = StateT NextMap WebMonad

deleteTab = ask >>= \conf -> liftIO $ do
  current <- currentView (tabs conf)
  deleted <- PL.delete `fmap` readIORef (tabs conf)
  case deleted of 
    Nothing -> Web.webViewLoadUri current "about:blank"
    Just x -> do
      GTK.widgetDestroy current
      writeIORef (tabs conf) x
      return ()
  updateView (showTabs conf) (container conf) (tabs conf)

newTab url = ask >>= \conf -> liftIO $ do
  webView <- Web.webViewNew
  Web.webViewLoadUri webView url
  modifyIORef (tabs conf) $ insertRight webView
  updateView (showTabs conf) (container conf) (tabs conf)


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

--submap :: (MonadReader WebConf m, MonadIO m) => Keymap (WebMonad ()) -> m ()
submap map = ask >>=( \(mapRef -> ref ) -> io $ modifyIORef ref (push map))
             >> put Keep

setKeymap :: (MonadReader WebConf m, MonadIO m) => Keymap -> m ()
setKeymap map =  ask >>= \(mapRef -> ref) -> io $ writeIORef ref [map]

modifyKeymap f =  ask >>= \(mapRef -> ref) -> io $ 
                    readIORef ref >>= writeIORef ref . f
  

loadURL url = do
  page <- io . currentView =<< asks tabs
  io $ Web.webViewLoadUri page url

myKeymap view bar = M.fromList
           [ ((0, GTK.keyFromName "l"), submap loadKeymap)
           , ((0, GTK.keyFromName "h"), io $ putStrLn "hello world" )
           , ((0, GTK.keyFromName "o"), io $ activateInputBar bar "http://"  )
           , ((0, GTK.keyFromName "n"), newTab "http://tagesschau.de"  )
           , ((0, GTK.keyFromName "x"), deleteTab )
           ]

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


handleKey webConf = do 
  keyVal <-  GTK.eventKeyVal
  mods <- fromFlags <$> GTK.eventModifier
  keymap <- io $ top <$> readIORef (mapRef webConf)
  case M.lookup (mods, keyVal) (map `M.union` keymap) of 
    Nothing -> return False
    Just action -> do
      (_, nextmap) <- io $ runReaderT (runStateT action Reset) webConf
      case nextmap of
        Reset -> io $ writeIORef (mapRef webConf) [keymap]
        Back  -> io $ modifyIORef (mapRef webConf) pop
        Keep  -> return ()
      return True
  where
    map = M.fromList [ ((0,GTK.keyFromName "q") , resetMap) ]
    resetMap = setKeymap (defaultMap webConf)

handleEntryKey bar view = do 
  keyName <-  GTK.eventKeyName
  mods <- fromFlags <$> GTK.eventModifier
  case (mods, keyName) of
    (0, "Escape") -> io $ deactivateInputBar bar view
    _                         -> return ()
  return False

deriving instance Show Web.LoadStatus

statusBarUpdate'
  :: (Web.WebViewClass o) => IORef (Maybe String, Maybe String) -> o -> ([Char] -> IO a) -> IO ()
statusBarUpdate' hovering webView update = do
  loadStatus   <- GTK.get webView Web.webViewLoadStatus
  loadProgress <- GTK.get webView Web.webViewProgress
  uri <- fromMaybe "" <$> GTK.get webView Web.webViewUri
  (hoveringTitle', hoveringURL') <- readIORef hovering
  let hoveringURL = maybe "" (\x -> "(" ++ x ++ ")") hoveringURL'
  let status = case loadStatus of 
        Web.LoadFinished -> "done"
        Web.LoadFailed  -> "failed"
        _               -> show (floor $ loadProgress *100) ++ "%"
  title        <- GTK.get webView Web.webViewTitle
  
  update $ concat [ "[", status, "]"
                  , hoveringURL
                  , fromMaybe "" title , " "
                  , uri]
  return ()


(<$$>) = fmap.fmap

notebookGrabCurrentPage notebook =
  GTK.notebookGetCurrentPage notebook >>=
  GTK.notebookGetNthPage notebook
  
getCurrentWebView tabs =  do
  scrolledWindow <-  notebookGrabCurrentPage tabs
  case scrolledWindow of 
    Just x  -> Web.castToWebView <$$> GTK.binGetChild (GTK.castToBin x)
    Nothing -> return Nothing
  
-- createWebView
--   :: Maybe [Char] -> IO (Web.WebView, GTK.ScrolledWindow)
-- createWebView url = do
--   webView <- Web.webViewNew
--   Web.webViewLoadUri webView (fromMaybe "about:blank" url)
--   scrollWindow <- GTK.scrolledWindowNew Nothing Nothing
--   GTK.containerAdd scrollWindow webView
--   return (webView, scrollWindow)

main = do
  GTK.initGUI
  
--  setProxy "http://localhost:8118/" -- TODO: uncomment
  
  window <- GTK.windowNew
  
  box <- GTK.vBoxNew False 0
  
  urlBar <- GTK.entryNew
  GTK.entrySetHasFrame urlBar False
  
  tabLabel <- GTK.labelNew $ Just "()"
  tabs <- containerNew
  
  webView <- currentView tabs
  
  hovering <- newIORef (Nothing, Nothing)
  
  scrollWindow <- GTK.scrolledWindowNew Nothing Nothing
  GTK.containerAdd scrollWindow webView
  
  statusbar <- GTK.labelNew Nothing
  GTK.set statusbar [ GTK.miscXalign  GTK.:= 0 ]
  
  Web.webViewLoadUri webView "http://google.com"
  
  GTK.containerAdd window box
  
  GTK.boxPackStart box tabLabel GTK.PackNatural 0
  GTK.boxPackStart box scrollWindow GTK.PackGrow    0
  GTK.boxPackStart box statusbar    GTK.PackNatural 0
  GTK.boxPackStart box urlBar       GTK.PackNatural 0  
  
  let log msg = GTK.labelSetText statusbar msg >> return ()
  let statusBarUpdate = statusBarUpdate' hovering webView log
  let keymap = myKeymap tabs urlBar 
  let showTabs = GTK.labelSetText tabLabel . show <=< titleList
      
  mapRef <- newIORef [keymap]
  
  webView' <- currentView tabs
  
  GTK.on scrollWindow GTK.keyPressEvent . handleKey $ WebConf mapRef tabs scrollWindow keymap showTabs
  GTK.on urlBar GTK.keyPressEvent $ handleEntryKey urlBar webView
  GTK.on webView Web.progressChanged . const $ statusBarUpdate
  
  GTK.on webView' Web.hoveringOverLink (\title url -> (writeIORef hovering (title, url) 
                                               >> statusBarUpdate))
    
  GTK.on urlBar GTK.entryActivate $ do
    url <- GTK.entryGetText urlBar
    Web.webViewLoadUri webView url
    deactivateInputBar urlBar scrollWindow  
  GTK.widgetShowAll window
--   GTK.widgetHide urlBar
  GTK.onDestroy window GTK.mainQuit
  deactivateInputBar urlBar scrollWindow
  GTK.mainGUI
