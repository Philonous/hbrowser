{-# LANGUAGE DeriveFunctor, NoMonomorphismRestriction, ViewPatterns, StandaloneDeriving #-}

module Web.HBrowser.ViewContainer where

import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import qualified Graphics.UI.Gtk.WebKit.WebWindowFeatures as Web
import qualified Graphics.UI.Gtk.WebKit.WebNavigationAction as Web
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
    Nothing -> return . not . typeThroughOnMissmatch . config $ web
    Just action -> do
      (_, nextmap) <- liftIO $ runReaderT (runStateT action Reset) web
      case nextmap of
        Reset -> liftIO $ writeIORef (keymapRef web) [keymap $ config web]
        Back  -> liftIO $ modifyIORef (keymapRef web) pop
        Keep  -> return ()
      return True

handleMouse :: Web -> GTK.EventM GTK.EButton Bool
handleMouse web = do
  button <- GTK.eventButton
  mods <- fromFlags <$> GTK.eventModifier
  curentMousemap <- liftIO $ top <$> readIORef (mousemapRef web)
  through <- liftIO $ readIORef $ typeThroughRef web
  if through then return False
    else case lookup (mods, button) curentMousemap of 
    Nothing -> return . not . mouseThroughOnMissmatch . config $ web
    Just action -> do
      (_, nextmap) <- liftIO $ runReaderT (runStateT action Reset) web
      case nextmap of
        Reset -> liftIO $ writeIORef (mousemapRef web) [mousemap $ config web]
        Back  -> liftIO $ modifyIORef (mousemapRef web) pop
        Keep  -> return ()
      return True

statusBarUpdate :: WebMonad ()
statusBarUpdate = do
  conf <- asks config
  liftIO . (showStatus conf) =<< renderStatus conf

deriving instance Show Web.NavigationReason

setupView webView = do
  web <- ask
  let updateBar = runReaderT updateBars web
  liftIO $ GTK.on webView GTK.keyPressEvent    $ handleKey web    
  liftIO $ GTK.on webView GTK.buttonPressEvent $ handleMouse web      
  liftIO $ GTK.on webView Web.progressChanged (const updateBar)
  liftIO . GTK.on webView Web.navigationPolicyDecisionRequested $ \_ _ x _ -> do
    reason <- Web.webNavigationActionGetReason x
    button <- Web.webNavigationActionGetButton x
    print (reason, button)
    return False
  liftIO $ GTK.on webView Web.hoveringOverLink (\title url -> 
    (writeIORef (hovering web) (title, url) 
    >> updateBar))
  return ()

createTab :: WebMonad Tab
createTab = do
  webView <- liftIO $ Web.webViewNew
  scrolledWindow <- liftIO $ GTK.scrolledWindowNew Nothing Nothing
  liftIO $ GTK.containerAdd scrolledWindow webView
  setupView webView
  return $ Tab webView scrolledWindow

containerNew = do
  webView <- Web.webViewNew
  scrolledWindow <- GTK.scrolledWindowNew Nothing Nothing
  GTK.containerAdd scrolledWindow webView
  let newTab = Tab webView scrolledWindow
  ref <- newIORef $ singleton newTab
  return (ref, newTab)

currentTab' tabs = getL focus `fmap` readIORef tabs
currentTab = asks tabs >>= liftIO . currentTab'

currentView' tabs =  tabView `fmap` currentTab' tabs
currentView = tabView `fmap` currentTab

withTabs f = do 
  tabRef <- asks tabs
  tabs <- liftIO $ readIORef tabRef
  f tabs

modifyTabs f = do 
  web <- ask
  liftIO $ modifyIORef (tabs web) f
  updateView
  
maybeModifyTabs f = do 
  web <- ask
  liftIO $ do 
    t <- liftIO . readIORef $ tabs web
    case f t of 
      Nothing -> putStrLn "modifyTabs failed"
      Just x -> writeIORef (tabs web) x
  updateView

nextTab = lift $ modifyTabs PL.next
prevTab = lift $ modifyTabs PL.previous

toList (PointedList xs a ys) = reverse xs ++ (a:ys)

titleList tabs = do 
  let views = fmap tabView tabs
  titles <- mapM Web.webViewGetTitle views
  urls   <- mapM Web.webViewGetUri   views
  return $ zipWith fromMaybe (fromMaybe "" `fmap` urls) titles

updateBars = do    
  conf <- asks config
  let zipperToList x = getL focus x : suffix x 
                     ++ reverse (reversedPrefix x)
  withTabs (liftIO . showTabs conf . zipperToList)
  statusBarUpdate

updateView :: WebMonad ()
updateView = do
  conf <- ask
  let cont = container conf
  scrolledWindow <- tabScrolledWindow `fmap` currentTab
  view <- tabView `fmap` currentTab
  liftIO $ do
    child <- GTK.binGetChild cont
    maybe (return ()) (GTK.containerRemove cont) child
    GTK.containerAdd cont scrolledWindow
    GTK.widgetShowAll scrolledWindow
    GTK.widgetGrabFocus view
  updateBars
  
withCurrentView f = currentView >>= f
withCurrentViewIO f = withCurrentView (liftIO . f)

currentUrl = withCurrentViewIO Web.webViewGetUri

hoveringLink = snd `fmap` readCurrent hovering

readCurrent field = do
  ref <- asks field
  liftIO $ readIORef ref
  
