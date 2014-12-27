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

initSDL :: HasIOErr m => [Word32] -> m ()
initSDL = decide' SDLInitError . SDL.init . foldl (.|.) 0

renderRect :: HasIOErr m => SDL.Renderer -> Rect -> m ()
renderRect rndr rct = decide' RenderError . with rct $ SDL.renderDrawRect rndr

mkWindowAndRenderer :: HasIOErr m => CInt -> CInt -> m (SDL.Window, SDL.Renderer,CInt)
mkWindowAndRenderer height width = chk . alloca $ \wP -> rF wP
  where
    chk = decide (^. _3 . to (/= 0)) SDLWinAndRndrCreationError
    rF wPtr = alloca $ \rPtr -> do
      err <- liftIO $ SDL.createWindowAndRenderer width height SDL.SDL_WINDOW_SHOWN wPtr rPtr
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
with2 f x y = liftIO $ with x (\x' -> with y (\y' -> f x' y'))

with3
  :: (Storable a, Storable b, Storable c, Monad m, MonadIO m)
  => (Ptr a -> Ptr b -> Ptr c -> IO d)
  -> a
  -> b
  -> c
  -> m d
with3 f x y z = liftIO $ with x (\x' -> with y (\y' -> with z (\z' -> f x' y' z')))

rectIntersect :: MonadIO m => Rect -> Rect -> m Bool
rectIntersect r1 r2 = with3 (SDL.intersectRect) r1 r2 overlap
  where overlap = Rect { rectX = 0,rectY = 0,rectW = 0,rectH = 0 }

