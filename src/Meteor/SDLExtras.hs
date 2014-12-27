{-# LANGUAGE TemplateHaskell #-}
module Meteor.SDLExtras where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types (Rect(..))

import Foreign.C.Types
import Foreign.Ptr
import Foreign (Storable,peek,alloca,with,maybePeek)

import Control.Lens (makePrisms,to,_1,_2,_3,(^.))
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Either (left,right)

import Meteor.Types

type RectX = CInt
type RectY = CInt
type RectW = CInt
type RectH = CInt

-- Make some useful prisms !
makePrisms ''Rect

runSDL :: (a -> Bool) -> SDLErr -> IO a -> El a
runSDL f e a = liftIO a >>= \r -> El $ if f r then left e else right r

runSDL' :: SDLErr -> IO CInt -> El ()
runSDL' e a = liftIO a >>= \res -> El $ if res < 0 then left e else right ()

initSDL :: [Word32] -> El ()
initSDL = runSDL' SDLInitError . SDL.init . foldl (.|.) 0

chk :: IO (SDL.Window, SDL.Renderer,CInt) -> El (SDL.Window,SDL.Renderer)
chk a = liftIO a >>= \b -> El $ if b ^. _3 . to (==0)
                          then right (b ^. _1, b ^. _2)
                          else left SDLWinAndRndrCreationError

renderRect :: SDL.Renderer -> Rect -> El ()
renderRect rndr rct = runSDL' RenderError . with rct $ SDL.renderDrawRect rndr

mkWindowAndRenderer :: CInt -> CInt -> El (SDL.Window, SDL.Renderer)
mkWindowAndRenderer height width = chk . alloca $ \wP -> rF wP
  where
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

