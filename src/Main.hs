{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import Foreign.C.Types

import Control.Lens ((#))
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.State (gets)
import Meteor.Types
import Meteor.SDLExtras

hgt :: CInt
hgt = 480

wdt :: CInt
wdt = 640

getRects :: [SDL.Rect]
getRects = [ _Rect # (wdt `div` 4, hgt `div` 4, wdt `div` 2, hgt `div` 2) 
           , _Rect # (wdt `div` 3, hgt `div` 3, wdt `div` 2, hgt)
           ]

getColours :: [(Word8,Word8,Word8,Word8)]
getColours = [(0x00,0xFF,0x00,0xFF),(0xFF,0x00,0xFF,0x00)]

renderWith :: El ()
renderWith = do
  rndrr <- gets _meteorRenderer
  rects <- gets _meteorRects
              
  let actors = zip rects getColours

  setColour rndrr 0x00 0x00 0x00 0xFF >> clearRender rndrr

  traverse_ (dRect rndrr) actors

  SDL.renderPresent rndrr

  where
    -- correct fuckin spelling :[
    clearRender rndr = decide' RenderClearError $ SDL.renderClear rndr
    setColour rndr r g b a = decide' ColourSetError $ SDL.setRenderDrawColor rndr r g b a

    dRect rndr (rct,(r,g,b,a)) = setColour rndr r g b a >> renderRect rndr rct

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
  repeatUntilComplete $ renderWith >> handleEvents
  where
    handleEvents = liftIO (pollEvent >>= handleE)

initialise :: Et (SDL.Window,SDL.Renderer)
initialise = do
  initSDL [SDL.SDL_INIT_VIDEO]
  (w,r,_) <- mkWindowAndRenderer hgt wdt
  return (w,r)

createMeteor :: IO (Either SDLErr MeteorS)
createMeteor = do
  eM <- runEitherT initialise 
  return $ mkMeteor <$> eM
  where
    mkMeteor (w,r) = MeteorS w r $ getRects
  
main :: IO ()
main = do
  mS <- createMeteor
  case mS of
    Left e -> dieE e
    Right m -> runEl m mainLoop >>= either dieE (\_ -> putStrLn "victory!")
