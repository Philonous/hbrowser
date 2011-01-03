{-# LANGUAGE NoMonomorphismRestriction #-}

module Web.HBrowser.Scripting where

import qualified Graphics.UI.Gtk.WebKit.WebView as Web
import Web.HBrowser.WebMonad
import Web.HBrowser.ViewContainer
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception
import System.IO
import System.IO.Error

runScript  = withCurrentViewIO . flip Web.webViewExecuteScript

runScriptFromFile scriptName = do
  web <- ask
  jsDir <- asks $ jsScriptDir . config
  view <- currentView
  let scriptFile = jsDir ++ "/" ++ scriptName
  liftIO . putStrLn $ "running script: " ++ scriptFile
  liftIO $ catchJust isFileError
   (do
     withFile scriptFile ReadMode  $ \handle -> do
       script <- hGetContents handle
       Web.webViewExecuteScript view script
   )
   (\e -> print (e::IOException))
 where isFileError e | isDoesNotExistError e = Just e
                     | isPermissionError   e = Just e
                     | otherwise             = Nothing



  
  