{-# LANGUAGE DeriveFunctor, NoMonomorphismRestriction, ViewPatterns #-}

module Web.HBrowser.ViewContainer where

import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import qualified Graphics.UI.Gtk.WebKit.WebWindowFeatures as Web
import qualified Graphics.UI.Gtk as GTK
import System.Glib.Flags

import Data.List.PointedList.Circular as PL
import Data.IORef
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Record.Label
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans


import Web.HBrowser.WebMonad as WM

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

handleKey :: Web -> GTK.EventM GTK.EKey Bool
handleKey web = do 
  keyVal <-  GTK.eventKeyVal
  mods <- fromFlags <$> GTK.eventModifier
  currentKeymap <- liftIO $ top <$> readIORef (keymapRef web)
  through <- liftIO $ readIORef $ typeThroughRef web
  if through then do
      when (keyVal == GTK.keyFromName "Escape") .
        liftIO $ writeIORef (typeThroughRef web) False
      return False
    else case M.lookup (mods, keyVal) currentKeymap of 
    Nothing -> return False
    Just action -> do
      (_, nextmap) <- liftIO $ runReaderT (runStateT action Reset) web
      case nextmap of
        Reset -> liftIO $ writeIORef (keymapRef web) [keymap $ config web]
        Back  -> liftIO $ modifyIORef (keymapRef web) pop
        Keep  -> return ()
      return True

statusBarUpdate :: WebMonad ()
statusBarUpdate = do
  conf <- asks config
  liftIO . (showStatus conf) =<< renderStatus conf

setupView webView = do
  web <- ask
  let updateBar = runReaderT updateBars web
  liftIO $ GTK.on webView GTK.keyPressEvent $ handleKey web    
  liftIO $ GTK.on webView Web.progressChanged (const updateBar)
  liftIO $ GTK.on webView Web.hoveringOverLink (\title url -> 
    (writeIORef (hovering web) (title, url) 
    >> updateBar))
  
  return ()

createView :: WebMonad Web.WebView
createView = do
  webView <- liftIO $ Web.webViewNew
  setupView webView
  return webView

containerNew = do
  webView <- Web.webViewNew
  ref <- newIORef $ singleton webView 
  return (ref, webView)

currentView = do 
  ref <- asks tabs
  liftIO $ getL focus `fmap` readIORef ref

currentView' tabs =  getL focus `fmap` readIORef tabs

withTabs f = do 
  tabRef <- asks tabs
  tabs <- liftIO $ readIORef tabRef
  f tabs

modifyTabs f = do 
  web <- ask
  liftIO $ modifyIORef (tabs web) f
  updateView
  
nextTab = lift $ modifyTabs PL.next
prevTab = lift $ modifyTabs PL.previous

toList (PointedList xs a ys) = reverse xs ++ (a:ys)

titleList tabs = do 
  titles <- mapM Web.webViewGetTitle tabs
  urls   <- mapM Web.webViewGetUri   tabs
  return $ zipWith fromMaybe (fromMaybe "" `fmap` urls) titles

updateBars = do    
  conf <- asks config
  let tabsToList x = getL focus x : suffix x 
                     ++ reverse (reversedPrefix x)
  withTabs (liftIO . showTabs conf . tabsToList)
  statusBarUpdate

updateView :: WebMonad ()
updateView = do
  conf <- ask
  let cont = container conf
  cur <- currentView 
  liftIO $ do
    child <- GTK.binGetChild cont
    maybe (return ()) (GTK.containerRemove cont) child
    GTK.containerAdd cont cur
    GTK.widgetShow cur
    GTK.widgetGrabFocus cur
  updateBars
  
withCurrentView f = currentView >>= f
withCurrentViewIO f = withCurrentView (liftIO . f)
