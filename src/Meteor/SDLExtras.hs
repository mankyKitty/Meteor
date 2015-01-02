{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Meteor.SDLExtras where

import Prelude ()
import BasePrelude hiding (left,right,bool)

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types (Rect(..))

import Foreign.C.Types
import Foreign.Ptr
import Foreign (Storable,peek,alloca,with,maybePeek)

import Control.Lens (makePrisms,to,_3,(^.),Traversal',Lens',lens)
import Control.Monad.IO.Class (MonadIO,liftIO)

import Meteor.Types
import Foreign.C (withCAString)

type RectX = CInt
type RectY = CInt
type RectW = CInt
type RectH = CInt

-- Make some useful prisms !
makePrisms ''Rect
-- Rect lenses
rectX' :: Lens' Rect CInt
rectX' = lens rectX (\rct x -> rct { rectX = x })

rectY' :: Lens' Rect CInt
rectY' = lens rectY (\rct y -> rct { rectY = y })

rectW' :: Lens' Rect CInt
rectW' = lens rectW (\rct w -> rct { rectW = w })

rectH' :: Lens' Rect CInt
rectH' = lens rectH (\rct h -> rct { rectH = h })

setRectPos :: Traversal' Rect CInt
setRectPos f (Rect x y w h) =
  Rect <$> f x <*> f y <*> pure w <*> pure h

bitOr :: [Word32] -> Word32
bitOr = foldl (.|.) 0

initSDL :: HasSDLErr m => [Word32] -> m ()
initSDL = decide' SDLInitError . SDL.init . bitOr

mkRenderer :: HasSDLErr m => SDL.Window -> m SDL.Renderer
mkRenderer w = decide isNullPtr RendererCreateError $ SDL.createRenderer w (-1) flags
  where
    flags = bitOr [SDL.SDL_RENDERER_ACCELERATED,SDL.SDL_RENDERER_PRESENTVSYNC]

mkWindow :: HasSDLErr m => String -> CInt -> CInt -> m SDL.Window
mkWindow t h w = decide isNullPtr WindowCreateError . withCAString t $ \title ->
                 SDL.createWindow title
                 SDL.SDL_WINDOWPOS_UNDEFINED
                 SDL.SDL_WINDOWPOS_UNDEFINED
                 w
                 h
                 flags
  where
    flags = bitOr [SDL.SDL_WINDOW_OPENGL,SDL.SDL_WINDOW_INPUT_GRABBED]
           
mkWindowAndRenderer :: HasSDLErr m => CInt -> CInt -> m (SDL.Window, SDL.Renderer,CInt)
mkWindowAndRenderer height width = chk . alloca $ \wP -> rF wP
  where
    chk = decide (^. _3 . to (/= 0)) SDLWinAndRndrCreationError
    rF wPtr = alloca $ \rPtr -> do
      err <- liftIO $ SDL.createWindowAndRenderer
             width height
             SDL.SDL_WINDOW_OPENGL
             wPtr rPtr
      winPtr <- peek wPtr
      renPtr <- peek rPtr
      return (winPtr,renPtr,err)

peek' :: (Storable a, Monad m, MonadIO m) => Ptr a -> m a
peek' = liftIO . peek

isNullPtr :: Ptr a -> Bool
isNullPtr = (== nullPtr)

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \ptr -> do
  status <- SDL.pollEvent ptr
  if status == 1
    then maybePeek peek ptr
    else return Nothing

handleE :: Maybe SDL.Event -> IO Bool
handleE st = return $ maybe False (
             \e ->
              case e of
               SDL.QuitEvent _ _ -> True
               _                 -> False
             ) st
  
applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer op' ptr = liftM op' $ peek ptr

with2
  :: (Storable a, Storable b, Monad m, MonadIO m)
  => (Ptr a -> Ptr b -> IO c)
  -> a
  -> b
  -> m c
with2 f x y = liftIO . with x $ with y . f

with3
  :: (Storable a, Storable b, Storable c, Monad m, MonadIO m)
  => (Ptr a -> Ptr b -> Ptr c -> IO d)
  -> a
  -> b
  -> c
  -> m d
with3 f x y z = liftIO $ with x (\x' -> with y $ with z . f x')

clearRender :: HasSDLErr m => SDL.Renderer -> m ()
clearRender rndr = decide' RenderClearError $ SDL.renderClear rndr

setColour :: HasSDLErr m => SDL.Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> m ()
setColour rndr r g b a = decide' ColourSetError $ SDL.setRenderDrawColor rndr r g b a

renderRect :: HasSDLErr m => SDL.Renderer -> Rect -> m ()
renderRect rndr rct = decide' RenderError . with rct $ SDL.renderDrawRect rndr
