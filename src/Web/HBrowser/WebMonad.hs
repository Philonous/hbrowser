module Web.HBrowser.WebMonad where

import qualified Graphics.UI.Gtk as GTK
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

type MouseDef = (ModifierFlags, Int)
type Mousemap = M.Map MouseDef (WebInputMonad ())

type Tabs = IORef (PointedList Web.WebView)

type WebMonad = ReaderT Web IO


data Web = Web 
  { keymapRef      :: MapRef
  , mousemapRef    :: IORef [Mousemap]
  , tabs           :: Tabs
  , container      :: GTK.ScrolledWindow
  , hovering       :: IORef (Maybe String, Maybe String)
  , config         :: WebConf
  , inputAction    :: IORef (String -> WebMonad ())
  , inputEntry     :: GTK.Entry
  , inputLabel     :: GTK.Label
  , inputBox       :: GTK.HBox
  , typeThroughRef :: IORef Bool
  }

data WebConf = WebConf 
  { keymap      :: Keymap 
  , mousemap    :: Mousemap
  , showTabs    :: [Web.WebView] -> IO ()
  , showStatus  :: String -> IO ()
  , renderStatus:: WebMonad String
  , homeURL     :: String
  , jsScriptDir :: String
  } 
  
