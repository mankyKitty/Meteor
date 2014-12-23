module Main where

import Prelude ()
import BasePrelude hiding (first)

import qualified Graphics.UI.SDL as SDL
import Data.Bifunctor (first)

import Foreign.C.Types

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT(..),runExceptT,throwError)

hgt :: CInt
hgt = 640

wdt :: CInt
wdt = 480

data SDLErr = SDLInitError
              deriving Show

runSDL :: (Monad m, MonadIO m) => ExceptT SDLErr m a -> m a
runSDL = _foo . runExceptT

main :: IO ()
main = do
  putStrLn "dooo eeet"
  -- init SDL video
  -- init and grab the window

  -- get the window surface
  -- set the surface format to use pixels
