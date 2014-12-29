{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import Control.Lens (Lens',(^.),_1,(&),(%~),(#),traversed,(-=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.State (gets,modify)

import Foreign.C.Types (CInt)

import Meteor.Types
import Meteor.SDLExtras
import Meteor.Core

renderWith :: El ()
renderWith = do
  -- Move all the missiles
  meteorMissiles . traversed . rectY' -= 2

  rndrr <- gets _meteorRenderer

  plyr <- gets _meteorPlayer
              
  -- set the background colour
  setColour rndrr 0x00 0x00 0x00 0xFF >> clearRender rndrr

  dRect rndrr plyr

  ms <- gets _meteorMissiles
  traverse_ (dRect rndrr . (,missileColour)) ms

  -- present the updates.
  SDL.renderPresent rndrr

  where
    dRect rndr (rct,c) = setColour rndr r g b a >> renderRect rndr rct
      where
        (r,g,b,a) = c ^. _Col

quitApp :: El ()
quitApp = do
  gets _meteorRenderer >>= SDL.destroyRenderer
  gets _meteorWindow >>= SDL.destroyWindow
  SDL.quit

repeatUntilComplete :: El Bool -> El ()
repeatUntilComplete op' = do
  fin <- op'
  unless fin $ repeatUntilComplete op'

dieE :: SDLErr -> IO ()
dieE e = putStrLn $ "ERROR: " <> show e

addMissile :: MeteorS -> [SDL.Rect] -> [SDL.Rect]
addMissile s ms = newM : ms
  where
    ppos :: Lens' SDL.Rect CInt -> CInt
    ppos l = s ^. meteorPlayer . _1 . l

    newM = _Rect # (ppos rectX',ppos rectY',15,15)

moveP :: SDL.Keysym -> MeteorS -> MeteorS
moveP k s = case SDL.keysymKeycode k of
  SDL.SDLK_LEFT -> lfr (subtract distance)
  SDL.SDLK_RIGHT -> lfr (+distance)
  SDL.SDLK_SPACE -> s & meteorMissiles %~ addMissile s
  _ -> s
  where
    distance = 10

    posUpdate lns g = s & meteorPlayer . _1 . lns %~
                      (\xV -> bnds screenWidth xV $ g xV)

    bnds sW o n = bool o n $ n > 0 && n < sW

    lfr = posUpdate rectX'

updatePlayer :: SDL.Event -> MeteorS -> MeteorS
updatePlayer (SDL.KeyboardEvent SDL.SDL_KEYDOWN _ _ _ _ kSym) m = moveP kSym m
updatePlayer _ m = m

updateActors :: Maybe SDL.Event -> El ()
updateActors = maybe (return ()) (modify . updatePlayer)

mainLoop :: El ()
mainLoop = repeatUntilComplete (renderWith >> handleInputs >> hExit)
  where
    hExit = liftIO (pollEvent >>= handleE)
    handleInputs = liftIO pollEvent >>= updateActors

initialise :: Et (SDL.Window,SDL.Renderer)
initialise = do
  initSDL [SDL.SDL_INIT_VIDEO]
  win <- mkWindow "Meteor!" screenHeight screenWidth
  rdr <- mkRenderer win
  return (win,rdr)

createMeteor :: IO (Either SDLErr MeteorS)
createMeteor = do
  eM <- runEitherT initialise 
  return $ mkMeteor <$> eM
  where
    mkMeteor (w,r) = MeteorS
                     w
                     r
                     getInitialPlayer
                     [] -- no missiles to start the game
                     [] -- no mobs to start just yet.
                     False
  
main :: IO ()
main = do
  mS <- createMeteor
  case mS of
    Left e -> dieE e
    Right m -> runEl m (mainLoop >> quitApp) >>= either dieE (\_ -> putStrLn "victory!")
