module Web.HBrowser.WebMonad where

import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import Data.Map as M
import Data.List.PointedList.Circular as PL 
import Data.IORef
import Control.Monad.Reader
import Control.Monad.State

data NextMap = Reset | Keep | Back 
type WebInputMonad = StateT NextMap WebMonad

type Keymap = M.Map KeyDef (WebInputMonad ())
type ModifierFlags = Int 
type KeyDef = (ModifierFlags, GTK.KeyVal)
type MapRef = IORef [Keymap ]

type Tabs = IORef (PointedList Web.WebView)

type WebMonad = ReaderT Web IO

data Web = Web 
  { mapRef    :: MapRef
  , tabs      :: Tabs
  , container :: GTK.ScrolledWindow
  , hovering  :: IORef (Maybe String, Maybe String)
  , config    :: WebConf
  }

data WebConf = WebConf 
  { keymap      :: Keymap 
  , showTabs    :: [Web.WebView] -> IO ()
  , showStatus  :: String -> IO ()
  , renderStatus:: WebMonad String
  , homeURL     :: String
  } 

