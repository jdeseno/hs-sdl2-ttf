#include "SDL_ttf.h"
module Graphics.UI.SDL.TTF
  ( FontStruct(..)
  , Font(..)
  , PointSize(..)
  , FontStyle(..)
  , FreeTypeHinting(..)
  , version
  , byteOrderNative
  , byteOrderSwapped
  , byteSwappedUnicode
  , init
  , openFont
  , openFontIndex
  , openFontRW
  , openFontIndexRW 
  , getFontStyle
  , setFontStyle
  , getFontOutline
  , setFontOutline
  , getFontHinting
  , setFontHinting
  , fontHeight
  , fontAscent
  , fontDescent
  , fontLineSkip
  , getFontKerning
  , setFontKerning
  , fontFaces
  , fontFaceIsFixedWidth
  , fontFaceFamilyName
  , fontFaceStyleName
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

data FontStyle
   = NORMAL | BOLD | ITALIC | UNDERLINE | STRIKETHROUGH
   deriving (Eq, Show)

fontStyleToConstant :: FontStyle -> #{type int}
fontStyleToConstant style =
  case style of
    NORMAL -> #{const TTF_STYLE_NORMAL}
    BOLD   -> #{const TTF_STYLE_BOLD}
    ITALIC -> #{const TTF_STYLE_ITALIC}
    UNDERLINE -> #{const TTF_STYLE_UNDERLINE}
    STRIKETHROUGH -> #{const TTF_STYLE_STRIKETHROUGH}

constantToFontStyle :: #{type int} -> FontStyle
constantToFontStyle #{const TTF_STYLE_NORMAL} = NORMAL
constantToFontStyle #{const TTF_STYLE_BOLD} = BOLD
constantToFontStyle #{const TTF_STYLE_ITALIC} = ITALIC
constantToFontStyle #{const TTF_STYLE_UNDERLINE} = UNDERLINE
constantToFontStyle #{const TTF_STYLE_STRIKETHROUGH} = STRIKETHROUGH
constantToFontStyle _ = error "(constantToFontStyle) unhandled font style"

foreign import ccall unsafe "TTF_GetFontStyle"
  ttfGetFontStyle' :: Ptr FontStruct -> IO #{type int}

getFontStyle :: Font -> IO FontStyle
getFontStyle font =
  withForeignPtr font $ \font' ->
    ttfGetFontStyle' font' >>= return . constantToFontStyle

foreign import ccall unsafe "TTF_SetFontStyle"
  ttfSetFontStyle' :: Ptr FontStruct -> #{type int} -> IO ()

setFontStyle :: Font -> FontStyle -> IO ()
setFontStyle font style =
  withForeignPtr font $ \font' ->
    ttfSetFontStyle' font' (fontStyleToConstant style)

foreign import ccall unsafe "TTF_GetFontOutline"
  ttfGetFontOutline' :: Ptr FontStruct -> IO #{type int}

getFontOutline :: Font -> IO Int
getFontOutline font =
  withForeignPtr font $ \font' ->
    ttfGetFontOutline' font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_SetFontOutline"
  ttfSetFontOutline' :: Ptr FontStruct -> #{type int} -> IO ()

setFontOutline :: Font -> Int -> IO ()
setFontOutline font size =
  withForeignPtr font $ \font' ->
    ttfSetFontOutline' font' (fromIntegral size)

data FreeTypeHinting
   = HintingNormal | HintingLight | HintingMono | HintingNone
   deriving (Show, Eq)

freeTypeHintingToConstant :: FreeTypeHinting -> #{type int}
freeTypeHintingToConstant HintingNormal = #{const TTF_HINTING_NORMAL}
freeTypeHintingToConstant HintingLight  = #{const TTF_HINTING_LIGHT}
freeTypeHintingToConstant HintingMono   = #{const TTF_HINTING_MONO}
freeTypeHintingToConstant HintingNone   = #{const TTF_HINTING_NONE}

constantToFreeTypeHinting :: #{type int} -> FreeTypeHinting
constantToFreeTypeHinting #{const TTF_HINTING_NORMAL} = HintingNormal
constantToFreeTypeHinting #{const TTF_HINTING_LIGHT}  = HintingLight
constantToFreeTypeHinting #{const TTF_HINTING_MONO}   = HintingMono
constantToFreeTypeHinting #{const TTF_HINTING_NONE}   = HintingNone
constantToFreeTypeHinting _ = error "(constantToFreeTypeHinting) unhandled hinting"

foreign import ccall unsafe "TTF_GetFontHinting"
  ttfGetFontHinting' :: Ptr FontStruct -> IO #{type int}

getFontHinting :: Font -> IO FreeTypeHinting
getFontHinting font =
  withForeignPtr font $ \font' ->
    ttfGetFontHinting' font' >>= return . constantToFreeTypeHinting

foreign import ccall unsafe "TTF_SetFontHinting"
  ttfSetFontHinting' :: Ptr FontStruct -> #{type int} -> IO ()

setFontHinting :: Font -> FreeTypeHinting -> IO ()
setFontHinting font hinting =
  withForeignPtr font $ \font' ->
    ttfSetFontHinting' font' (freeTypeHintingToConstant hinting)

foreign import ccall unsafe "TTF_FontHeight"
  ttfFontHeight' :: Ptr FontStruct -> IO #{type int}

fontHeight :: Font -> IO Int
fontHeight font =
  withForeignPtr font $ \font' ->
    ttfFontHeight' font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_FontAscent"
  ttfFontAscent :: Ptr FontStruct -> IO #{type int}

fontAscent :: Font -> IO Int
fontAscent font =
  withForeignPtr font $ \font' ->
    ttfFontAscent font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_FontDescent"
  ttfFontDescent' :: Ptr FontStruct -> IO #{type int}

fontDescent :: Font -> IO Int
fontDescent font =
  withForeignPtr font $ \font' ->
    ttfFontDescent' font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_FontLineSkip"
  ttfFontLineSkip' :: Ptr FontStruct -> IO #{type int}

fontLineSkip :: Font -> IO Int
fontLineSkip font =
  withForeignPtr font $ \font' ->
    ttfFontLineSkip' font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_GetFontKerning"
  ttfGetFontKerning' :: Ptr FontStruct -> IO #{type int}

getFontKerning :: Font -> IO Bool
getFontKerning font =
  withForeignPtr font $ \font' ->
    ttfGetFontKerning' font' >>= return . toBool

foreign import ccall unsafe "TTF_GetFontKerning"
  ttfSetFontKerning' :: Ptr FontStruct -> #{type int} -> IO ()

setFontKerning :: Font -> Bool -> IO ()
setFontKerning font dokerning =
  withForeignPtr font $ \font' ->
    ttfSetFontKerning' font' (fromBool dokerning)

foreign import ccall unsafe "TTF_FontFaces"
  ttfFontFaces' :: Ptr FontStruct -> IO #{type long}

fontFaces :: Font -> IO Int
fontFaces font =
  withForeignPtr font $ \font' ->
    ttfFontFaces' font' >>= return . fromIntegral

foreign import ccall unsafe "TTF_FontFaceIsFixedWidth"
  ttfFontFaceIsFixedWidth' :: Ptr FontStruct -> IO #{type int}

fontFaceIsFixedWidth :: Font -> IO Bool
fontFaceIsFixedWidth font =
  withForeignPtr font $ \font' ->
    ttfFontFaceIsFixedWidth' font' >>= return . toBool

foreign import ccall unsafe "TTF_FontFaceFamilyName"
  ttfFontFaceFamilyName' :: Ptr FontStruct -> IO CString

fontFaceFamilyName :: Font -> IO String
fontFaceFamilyName font =
  withForeignPtr font $ \font' ->
    ttfFontFaceFamilyName' font' >>= peekCString

foreign import ccall unsafe "TTF_FontFaceStyleName"
  ttfFontFaceStyleName' :: Ptr FontStruct -> IO CString

fontFaceStyleName :: Font -> IO String
fontFaceStyleName font =
  withForeignPtr font $ \font' ->
    ttfFontFaceStyleName' font' >>= peekCString

