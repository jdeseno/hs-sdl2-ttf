#include "SDL_ttf.h"
module Graphics.UI.SDL.TTF
  ( FontStruct(..)
  , Font(..)
  , PointSize(..)
  , version
  , byteOrderNative
  , byteOrderSwapped
  , byteSwappedUnicode
  , init
  , openFont
  , openFontIndex
  , openFontRW
  , openFontIndexRW 
  ) where

import Foreign
import Foreign.C.String
import Prelude hiding (init)
import Graphics.UI.SDL.Types

-- | (Major, Minor, Patchlevel)
version :: (Int, Int, Int)
version = ( #{const SDL_TTF_MAJOR_VERSION}
          , #{const SDL_TTF_MINOR_VERSION}
          , #{const SDL_TTF_PATCHLEVEL}
          )

newtype ByteOrderMark
      = ByteOrderMark { unwrapByteOrderMark :: #{type int} }

byteOrderNative :: ByteOrderMark
byteOrderNative = ByteOrderMark #{const UNICODE_BOM_NATIVE}

byteOrderSwapped :: ByteOrderMark
byteOrderSwapped = ByteOrderMark #{const UNICODE_BOM_SWAPPED}

foreign import ccall unsafe "TTF_ByteSwappedUNICODE"
  ttfByteSwappedUnicode' :: #{type int} -> IO ()

byteSwappedUnicode :: ByteOrderMark -> IO ()
byteSwappedUnicode bom =
  ttfByteSwappedUnicode' (unwrapByteOrderMark bom)

data FontStruct
type Font = ForeignPtr FontStruct

foreign import ccall unsafe "&TTF_CloseFont"
  ttfCloseFont_finalizer' :: FunPtr (Ptr FontStruct -> IO ())

mkFinalizedFont :: Ptr FontStruct -> IO Font
mkFinalizedFont = newForeignPtr ttfCloseFont_finalizer'

foreign import ccall unsafe "TTF_Init"
  ttfInit :: IO #{type int}

init :: IO ()
init = do
  ret <- ttfInit
  if ret == -1
  then error "(TTF) init failure"
  else return ()

type PointSize = Int

foreign import ccall unsafe "TTF_OpenFont"
  ttfOpenFont' :: CString -> #{type int} -> IO (Ptr FontStruct)

openFont :: String -> PointSize -> IO Font
openFont file ptsize =
  withCString file $ \file' ->
    ttfOpenFont' file' (fromIntegral ptsize) >>= mkFinalizedFont

foreign import ccall unsafe "TTF_OpenFontIndex"
  ttfOpenFontIndex' :: CString -> #{type int} -> #{type long} -> IO (Ptr FontStruct)

openFontIndex :: String -> PointSize -> Int -> IO Font
openFontIndex file ptsize index =
  withCString file $ \file' ->
    let ptsize' = fromIntegral ptsize
        index'  = fromIntegral index
    in ttfOpenFontIndex' file' ptsize' index' >>= mkFinalizedFont 

foreign import ccall unsafe "TTF_OpenFontRW"
  ttfOpenFontRW' :: Ptr RWopsStruct -> #{type int} -> #{type int} -> IO (Ptr FontStruct)

-- | Boolean option is to free source after loading
openFontRW :: RWops -> Bool -> PointSize -> IO Font
openFontRW rwops dofree ptsize =
  withForeignPtr rwops $ \rwops' ->
    ttfOpenFontRW' rwops' (fromBool dofree) (fromIntegral ptsize) >>=
      mkFinalizedFont

foreign import ccall unsafe "TTF_OpenFontIndexRW"
  ttfOpenFontIndexRW' :: Ptr RWopsStruct -> #{type int} -> #{type int} -> #{type long} -> IO (Ptr FontStruct)

-- | Boolean option is to free source after loading
openFontIndexRW :: RWops -> Bool -> PointSize -> Int -> IO Font
openFontIndexRW rwops dofree ptsize index =
  withForeignPtr rwops $ \rwops' ->
    let dofree' = fromBool dofree
        ptsize' = fromIntegral ptsize
        index'  = fromIntegral ptsize
    in ttfOpenFontIndexRW' rwops' dofree' ptsize' index' >>= mkFinalizedFont

