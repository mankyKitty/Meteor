{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as Map

import Control.Lens (Lens',(^.),_1,(&),(%~),(#),traversed,(-=),to,(%=),(+=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.State (gets,modify)

import Foreign.C.Types (CInt)

import Meteor.Types
import Meteor.SDLExtras
import Meteor.Core

removeHitMobs :: El ()
removeHitMobs = do
  -- Remove any mobs hit with missile
  enemies <- gets _meteorMobs
  bullets <- gets _meteorMissiles
  let hits = getHits enemies bullets

  maybe (return ())
    (\(mobIds,bIds) -> do
        meteorMobs %= Map.filterWithKey (\k _ -> k `notElem` mobIds)
        meteorMissiles %= Map.filterWithKey (\k _ -> k `notElem` bIds)
    )
    hits
  
renderWith :: El ()
renderWith = do
  ts <- SDL.getTicks
  -- fetch our renderer
  rndrr <- gets _meteorRenderer
  -- FIRE ZE MISSILEZ!
  -- Remove all offscreen missiles
  -- Move all the missiles
  meteorMissiles %= Map.filter (^. actorRect . rectY' . to (>= 0))
  meteorMissiles . traversed . actorRect . rectY' -= 2
  -- Move all enemies down a rank
  when (ts `mod` 12 == 0) $ meteorMobs . traversed . actorRect . rectY' += 2

  removeHitMobs
  -- Get our player
  plyr <- gets _meteorPlayer
              
  -- set the background colour
  setColour rndrr 0x00 0x00 0x00 0xFF >> clearRender rndrr

  -- Render the player on the screen
  drawRect rndrr plyr

  -- Get our missiles
  ms <- gets _meteorMissiles
  mobs <- gets _meteorMobs
  -- Render all missiles
  traverse_ (drawRect rndrr . (,missileColour)) ms
  traverse_ (drawRect rndrr . (,mobColour)) mobs

  -- present the updates.
  SDL.renderPresent rndrr


drawRect :: SDL.Renderer -> (Actor,Col) -> El ()
drawRect rndr (actor,c) = setColour rndr r g b a >> renderRect rndr rct
  where
    rct = actor ^. actorRect
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

addMissile :: MeteorS -> ActorMap -> ActorMap
addMissile s ms = Map.insert nextId newM ms
  where
    nextId = succ $ Map.size ms
    ppos :: Lens' SDL.Rect CInt -> CInt
    ppos l = s ^. meteorPlayer . _1 . actorRect . l

    newM = Actor (_Rect # (ppos rectX',ppos rectY',15,15))
           Bullet

moveP :: SDL.Keysym -> MeteorS -> MeteorS
moveP k s = case SDL.keysymKeycode k of
  SDL.SDLK_LEFT -> lfr (subtract distance)
  SDL.SDLK_RIGHT -> lfr (+distance)
  SDL.SDLK_SPACE -> s & meteorMissiles %~ addMissile s
  _ -> s
  where
    distance = 10

    posUpdate lns g = s & meteorPlayer . _1 . actorRect . lns %~
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
                     emptyBullets -- no missiles to start the game
                     getInitialMobs
                     False

emptyBullets :: ActorMap
emptyBullets = Map.empty

main :: IO ()
main = do
  mS <- createMeteor
  case mS of
    Left e -> dieE e
    Right m -> runEl m (mainLoop >> quitApp) >>= either dieE (\_ -> putStrLn "victory!")
