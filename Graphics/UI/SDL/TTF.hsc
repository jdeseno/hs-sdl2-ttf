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
  , glyphIsProvided
  , glyphMetrics
  , sizeText
  , sizeUTF8
  , renderTextSolid
  , renderUTF8Solid
  , renderTextShaded
  , renderUTF8Shaded
  , renderTextBlended
  , renderUTF8Blended
  , renderTextBlendedWrapped 
  , renderUTF8BlendedWrapped 
  ) where

import Foreign
import Foreign.C.String
import Prelude hiding (init)
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Color (Color(..))
import Graphics.UI.SDL.Raw (mkFinalizedSurface)

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

foreign import ccall unsafe "TTF_GlyphIsProvided"
  ttfGlyphIsProvided' :: Ptr FontStruct -> #{type Uint16} -> IO #{type int}

glyphIsProvided :: Font -> #{type Uint16} -> IO Bool
glyphIsProvided font glyph =
  withForeignPtr font $ \font' ->
    ttfGlyphIsProvided' font' glyph >>= return . toBool

foreign import ccall unsafe "TTF_GlyphMetrics"
  ttfGlyphMetrics' :: Ptr FontStruct -> #{type Uint16} ->
                      Ptr #{type int} -> Ptr #{type int} ->
                      Ptr #{type int} -> Ptr #{type int} ->
                      Ptr #{type int} -> IO #{type int}

-- | (minx, maxx, miny, maxy, advanced, index)
glyphMetrics :: Font -> #{type Uint16} -> IO (Int, Int, Int, Int, Int, Int)
glyphMetrics font glyph =
  alloca $ \minx' ->
  alloca $ \maxx' ->
  alloca $ \miny' ->
  alloca $ \maxy' ->
  alloca $ \advanced' ->
  withForeignPtr font $ \font' -> do
    index' <- ttfGlyphMetrics' font' glyph minx' maxx' miny' maxy' advanced'
    minx <- fmap fromIntegral $ peek minx'
    maxx <- fmap fromIntegral $ peek maxx'
    miny <- fmap fromIntegral $ peek miny'
    maxy <- fmap fromIntegral $ peek maxy'
    advanced <- fmap fromIntegral $ peek advanced'
    let index = fromIntegral index'
    return (minx, maxx, miny, maxy, advanced, index)

foreign import ccall unsafe "TTF_SizeText"
  ttfSizeText' :: Ptr FontStruct -> CString -> Ptr #{type int} -> Ptr #{type int} -> IO #{type int}

-- | returns (width, height)
sizeText :: Font -> String -> IO (Int, Int)
sizeText font text =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  alloca $ \w' ->
  alloca $ \h' -> do
    ret <- ttfSizeText' font' text' w' h'
    if ret == 0
    then do { w <- peek w' ; h <- peek h' ; return (fromIntegral w, fromIntegral h) }
    else error "sizeText"

foreign import ccall unsafe "TTF_SizeUTF8"
  ttfSizeUTF8' :: Ptr FontStruct -> CString -> Ptr #{type int} -> Ptr #{type int} -> IO #{type int}

-- | returns (width, height)
sizeUTF8 :: Font -> String -> IO (Int, Int)
sizeUTF8 font text =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  alloca $ \w' ->
  alloca $ \h' -> do
    ret <- ttfSizeUTF8' font' text' w' h'
    if ret == 0
    then do { w <- peek w' ; h <- peek h' ; return (fromIntegral w, fromIntegral h) }
    else error "sizeTextUTF8"

-- TODO TTF_SizeUNICODE

foreign import ccall unsafe "TTF_RenderText_Solid"
  ttfRenderTextSolid' :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)

renderTextSolid :: Font -> String -> Color -> IO Surface
renderTextSolid font text color =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with color $ \color' ->
    ttfRenderTextSolid' font' text' color' >>= mkFinalizedSurface

foreign import ccall unsafe "TTF_RenderUTF8_Solid"
  ttfRenderUTF8Solid' :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)

renderUTF8Solid :: Font -> String -> Color -> IO Surface
renderUTF8Solid font text color =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with color $ \color' ->
    ttfRenderUTF8Solid' font' text' color' >>= mkFinalizedSurface

-- TODO TTF_RenderUNICODE_Solid
-- TODO TTF_RenderGlyphSolid

foreign import ccall unsafe "TTF_RenderText_Shaded"
  ttfRenderTextShaded' :: Ptr FontStruct -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct)

renderTextShaded :: Font -> String -> Color -> Color -> IO Surface
renderTextShaded font text fg bg =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with fg $ \fg' ->
  with bg $ \bg' ->
    ttfRenderTextShaded' font' text' fg' bg' >>= mkFinalizedSurface

foreign import ccall unsafe "TTF_RenderUTF8_Shaded"
  ttfRenderUTF8Shaded' :: Ptr FontStruct -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct)

renderUTF8Shaded :: Font -> String -> Color -> Color -> IO Surface
renderUTF8Shaded font text fg bg =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with fg $ \fg' ->
  with bg $ \bg' ->
    ttfRenderUTF8Shaded' font' text' fg' bg' >>= mkFinalizedSurface

-- TODO TTF_RenderUNICODE_Shaded
-- TODO TTF_RenderGlyph_Shaded

foreign import ccall unsafe "TTF_RenderText_Blended"
  ttfRenderTextBlended' :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)

renderTextBlended :: Font -> String -> Color -> IO Surface
renderTextBlended font text color =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with color $ \color' ->
    ttfRenderTextBlended' font' text' color' >>= mkFinalizedSurface

foreign import ccall unsafe "TTF_RenderUTF8_Blended"
  ttfRenderUTF8Blended' :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)

renderUTF8Blended :: Font -> String -> Color -> IO Surface
renderUTF8Blended font text color =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with color $ \color' ->
    ttfRenderUTF8Blended' font' text' color' >>= mkFinalizedSurface

-- TODO TTF_RenderUNICODE_Blended

foreign import ccall unsafe "TTF_RenderText_Blended_Wrapped"
  ttfRenderTextBlendedWrapped' :: Ptr FontStruct -> CString -> Ptr Color -> #{type Uint32} -> IO (Ptr SurfaceStruct)

renderTextBlendedWrapped :: Font -> String -> Color -> Int -> IO Surface
renderTextBlendedWrapped font text fg wraplen =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with fg $ \fg' ->
    ttfRenderTextBlendedWrapped' font' text' fg' (fromIntegral wraplen) >>=
      mkFinalizedSurface

foreign import ccall unsafe "TTF_RenderUTF8_Blended_Wrapped"
  ttfRenderUTF8BlendedWrapped' :: Ptr FontStruct -> CString -> Ptr Color -> #{type Uint32} -> IO (Ptr SurfaceStruct)

renderUTF8BlendedWrapped :: Font -> String -> Color -> Int -> IO Surface
renderUTF8BlendedWrapped font text fg wraplen =
  withForeignPtr font $ \font' ->
  withCString text $ \text' ->
  with fg $ \fg' ->
    ttfRenderUTF8BlendedWrapped' font' text' fg' (fromIntegral wraplen) >>=
      mkFinalizedSurface

-- TODO TTF_RenderUNICODE_Blended_wrapped
-- TODO TTF_RenderGlyph_Blended_wrapped


