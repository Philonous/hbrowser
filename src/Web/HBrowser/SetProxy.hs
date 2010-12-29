{-# Language ForeignFunctionInterface #-}

module Web.HBrowser.SetProxy where

import Foreign.C 

foreign import ccall "setProxy" setProxy' :: CString -> IO ()

setProxy :: String -> IO ()
setProxy = flip withCString setProxy'