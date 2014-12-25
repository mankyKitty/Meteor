{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign (Storable,peek)

import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Either (EitherT(..),runEitherT,left,right)

hgt :: CInt
hgt = 640

wdt :: CInt
wdt = 480

newtype El a = El
  { unEl :: EitherT SDLErr IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             )

data SDLErr
  = SDLInitError
  | WindowCreationError
  deriving Show

runSDLAction :: (a -> Bool) -> SDLErr -> IO a -> El a
runSDLAction f e a = liftIO a >>= \r -> El $ if f r then left e else right r

initSDL :: [Word32] -> El CInt
initSDL = runSDLAction (<0) SDLInitError . SDL.init . foldl (.|.) 0

createWin :: String -> El SDL.Window
createWin t = runSDLAction isNullPtr WindowCreationError . withCAString t
              $ \t' -> SDL.createWindow t' posUndef posUndef hgt wdt SDL.SDL_WINDOW_SHOWN
  where
    posUndef = SDL.SDL_WINDOWPOS_UNDEFINED

isNullPtr :: Ptr a -> Bool
isNullPtr = (==nullPtr)

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer op ptr = liftM op $ peek ptr

elGrande :: (Monad m, MonadIO m) => El a -> m (Either SDLErr a)
elGrande = liftIO . runEitherT . unEl

main :: IO ()
main = do
  -- init SDL video
  -- init and grab the window
  eW <- elGrande $initSDL [SDL.SDL_INIT_VIDEO] >> createWin "Main"
  either print runApp eW

  where
    runApp w = do
      -- get the window surface
      surface <- SDL.getWindowSurface w
      pixelFormat <- SDL.surfaceFormat `applyToPointer` surface
      colour <- SDL.mapRGB pixelFormat 0x00 0xFF 0xFF

      _ <- SDL.fillRect surface nullPtr colour
      _ <- SDL.updateWindowSurface w
      SDL.delay 2000

      SDL.destroyWindow w
      SDL.quit
