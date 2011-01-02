{-# LANGUAGE ForeignFunctionInterface #-}

module Web.HBrowser.WebViewInternals
       ( castToWebView )
       where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import System.Glib.GType  (GType, typeInstanceIsA)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr)
import Graphics.UI.GtkInternals

castToWebView :: GObjectClass obj => obj -> WebView
castToWebView = castTo gTypeWebView "WebView"

castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


foreign import ccall unsafe "webkit_web_view_get_type" gTypeWebView :: GType
