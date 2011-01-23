{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Web.HBrowser.WebMonad where

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk (set, AttrOp((:=)), on)
import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import Data.Map as M
import Data.List.PointedList.Circular as PL 
import Data.IORef
import Control.Monad.Reader
import Control.Monad.State

data NextMap = Reset -- reset Map to default
             | Keep  -- keep current map
             | Back  -- revert one layer

type WebInputMonad = StateT NextMap WebMonad

type Keymap = M.Map KeyDef (WebInputMonad ())
type ModifierFlags = Int 
type KeyDef = (ModifierFlags, GTK.KeyVal)
type MapRef = IORef [Keymap]

type MouseDef = (ModifierFlags, GTK.MouseButton)
type Mousemap = [(MouseDef, WebInputMonad ())]

data Tab = Tab 
    { tabView    :: Web.WebView
    , tabScrolledWindow :: GTK.ScrolledWindow
    }
  
type Tabs = IORef (PointedList Tab)

type WebMonad = ReaderT Web IO

class (MonadReader Web m, MonadIO m, Functor m) => MonadWeb m
instance (MonadReader Web m, MonadIO m, Functor m) => MonadWeb m

data Web = Web 
  { keymapRef      :: MapRef
  , mousemapRef    :: IORef [Mousemap]
  , tabs           :: Tabs
  , container      :: GTK.EventBox
  , hovering       :: IORef (Maybe String, Maybe String)
  , config         :: WebConf
  , inputAction    :: IORef (String -> WebMonad ())
  , inputEntry     :: GTK.Entry
  , inputLabel     :: GTK.Label
  , inputBox       :: GTK.HBox
  , infoLabel      :: GTK.Label
  , typeThroughRef :: IORef Bool
  }

data WebConf = WebConf 
  { keymap      :: Keymap 
  , mousemap    :: Mousemap
  , showTabs    :: [Tab] -> IO ()
  , showStatus  :: String -> IO ()
  , renderStatus:: WebMonad String
  , homeURL     :: String
  , jsScriptDir :: String
  , typeThroughOnMissmatch :: Bool
  , mouseThroughOnMissmatch :: Bool
  } 
  
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

--hoverURL :: WebInputMonad (Maybe String)
hoverURL = do 
  hoveringRef <- asks hovering
  liftIO . fmap fst $ readIORef hoveringRef
  

