{-# LANGUAGE DeriveFunctor, NoMonomorphismRestriction, ViewPatterns #-}

module Web.HBrowser.ViewContainer where

import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import qualified Graphics.UI.Gtk as GTK
import Data.List.PointedList.Circular as PL
import Data.IORef
import Control.Monad
import Data.Maybe
import Data.Record.Label
import Control.Monad.Reader
import Control.Monad.Trans

import Web.HBrowser.WebMonad as WM


statusBarUpdate :: Web.WebView -> WebMonad ()
statusBarUpdate this = do
  cur <- currentView
  conf <- asks config
  when (this == cur) $
    liftIO . (showStatus conf) =<< renderStatus conf

setupView webView = do
  web <- ask 
  let updateBar = runReaderT (statusBarUpdate webView) web
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
  web <- ask
  liftIO $ modifyIORef (tabs web) f
  updateView

nextTab = lift $ withTabs PL.next
prevTab = lift $ withTabs PL.previous

toList (PointedList xs a ys) = reverse xs ++ (a:ys)

titleList tabs = do 
  titles <- mapM Web.webViewGetTitle tabs
  urls   <- mapM Web.webViewGetUri   tabs
  return $ zipWith fromMaybe (fromMaybe "" `fmap` urls) titles
  
updateView :: WebMonad ()
updateView = do
  conf <- ask 
  cur <- currentView 
  liftIO $ do
    child <- GTK.binGetChild (container conf)
    maybe (return ()) (GTK.containerRemove (container conf)) child
    GTK.containerAdd (container conf) cur
    GTK.widgetShow cur
    GTK.widgetGrabFocus cur
    (showTabs $ config conf) . (\x -> getL focus x : suffix x ++ reverse (reversedPrefix x)) =<< readIORef (tabs conf)
  statusBarUpdate cur



  