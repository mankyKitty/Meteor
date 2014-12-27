{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import Foreign.C.Types

import Control.Lens ((#))
import Control.Monad.IO.Class (liftIO,MonadIO)

import Meteor.Types
import Meteor.SDLExtras

hgt :: CInt
hgt = 480

wdt :: CInt
wdt = 640

renderWith :: (SDL.Window,SDL.Renderer) -> El ()
renderWith (_,rndrr) = do
  -- get the window surface
  let rectA = _Rect # (wdt `div` 4, hgt `div` 4, wdt `div` 2, hgt `div` 2)
  let rectB = _Rect # (wdt `div` 3, hgt `div` 3, wdt `div` 2, hgt)
              
  setColour 0x00 0x00 0x00 0xFF >> clearRender
  setColour 0x00 0xFF 0x00 0xFF >> dRect rectA
  setColour 0xFF 0x00 0xFF 0x00 >> dRect rectB

  SDL.renderPresent rndrr

  where
    -- correct fuckin spelling :[
    clearRender = runSDL' RenderClearError $ SDL.renderClear rndrr
    setColour r g b a = runSDL' ColourSetError $ SDL.setRenderDrawColor rndrr r g b a
    dRect = renderRect rndrr

quitApp :: (SDL.Window,SDL.Renderer) -> IO ()
quitApp (w,r) = SDL.destroyRenderer r >> SDL.destroyWindow w >> SDL.quit

repeatUntilComplete :: (Monad m, MonadIO m) => m Bool -> m ()
repeatUntilComplete op' = do
  complete <- op'
  unless complete $ repeatUntilComplete op'

dieE :: SDLErr -> IO ()
dieE e = putStrLn $ "ERROR: " <> show e

mainLoop :: El ()
mainLoop = do
  initSDL [SDL.SDL_INIT_VIDEO] 
  wR <- mkWindowAndRenderer hgt wdt
  repeatUntilComplete $ renderWith wR >> handleEvents
  where
    handleEvents = liftIO (pollEvent >>= handleE)

main :: IO ()
main = elGrande mainLoop >>= either dieE (\_ -> putStrLn "victory!")
