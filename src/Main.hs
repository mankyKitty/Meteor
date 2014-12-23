{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import BasePrelude

import qualified Graphics.UI.SDL as SDL

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign (Storable,peek)

import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Except (ExceptT(..),runExceptT,throwError,MonadError(..))

hgt :: CInt
hgt = 640

wdt :: CInt
wdt = 480

type EEL m a = ExceptT SDLErr m a

data SDLErr
  = SDLInitError
  | WindowCreationError
  deriving Show

runSDLAction :: (Monad m, MonadIO m) => (a -> Bool) -> SDLErr -> IO a -> ExceptT SDLErr m a
runSDLAction f e a = liftIO a >>= \r -> if f r then return r else throwError e

initSDL :: (Monad m, MonadIO m) => [Word32] -> ExceptT SDLErr m CInt
initSDL = runSDLAction (not . (<0)) SDLInitError . SDL.init . foldl (.|.) 0

createWin :: (Monad m, MonadIO m) => String -> ExceptT SDLErr m SDL.Window
createWin t = runSDLAction (not . (==nullPtr)) WindowCreationError . withCAString t
              $ \t' -> SDL.createWindow t' posUndef posUndef hgt wdt SDL.SDL_WINDOW_SHOWN
  where
    posUndef = SDL.SDL_WINDOWPOS_UNDEFINED

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer op ptr = liftM op $ peek ptr

main :: IO ()
main = do
  putStrLn "dooo eeet"
  -- init SDL video
  -- init and grab the window
  eW <- runExceptT $ (initSDL [SDL.SDL_INIT_VIDEO] >> createWin "Main")
  either print runApp eW

  where
    runApp w = do
      -- get the window surface
      surface <- SDL.getWindowSurface w
      pixelFormat <- SDL.surfaceFormat `applyToPointer` surface
      colour <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF

      _ <- SDL.fillRect surface nullPtr colour
      _ <- SDL.updateWindowSurface w
      SDL.delay 2000

      SDL.destroyWindow w
      SDL.quit
