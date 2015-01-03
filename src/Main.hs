{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map

import Control.Lens ( (^.),_1,(#),(-=)
                    , traversed,to,(+=),(%=)
                    , use,_Just,view,(.=)
                    )

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Meteor.Types
import Meteor.SDLExtras
import Meteor.Core

removeHitMobs :: El ()
removeHitMobs = do
    hits <- getHits <$> use meteorMobs <*> use meteorMissiles
    _ <- _Just h hits
    return ()
  where
    w xs k _ = notElem k xs

    h (mIds,bIds) = meteorMobs %= f mIds >> meteorMissiles %= g bIds
    g ids = V.ifilter (w ids)
    f ids = Map.filterWithKey (w ids)
 
missileOffScreen :: HasRect a => a -> Bool
missileOffScreen = (>=0) . view rectY' . getRect

renderWith :: El ()
renderWith = do
  ts <- SDL.getTicks
  -- Remove all offscreen missiles
  meteorMissiles %= V.filter missileOffScreen
  -- Move all the missiles
  meteorMissiles . traversed . rectY' -= 4
  -- Move all enemies down a rank
  when (ts `mod` 12 == 0) $ meteorMobs . traversed . actorRect . rectY' += 2
  -- remove struck mobs and missiles
  removeHitMobs
  -- set the background colour
  use meteorRenderer >>= \r -> setColour r 0x00 0x00 0x00 0xFF >> clearRender r
  -- Render the player on the screen
  use meteorPlayer >>= uncurry d'
  -- traverse the Vector and run monadic actions
  use meteorMissiles >>= traverse_ dMis
  use meteorMobs >>= traverse_ dMob

  -- present the updates.
  use meteorRenderer >>= SDL.renderPresent
  where
    d' m c = use meteorRenderer >>= \r -> drawRectable r m c
    dMob m = d' m mobColour
    dMis m = d' m missileColour

drawRectable :: HasRect a => SDL.Renderer -> a -> Col -> El ()
drawRectable rndr x c = setColour rndr r g b a >> renderRect rndr rct
  where
    (r,g,b,a) = c ^. _Col
    rct = getRect x

quitApp :: El ()
quitApp = do
  use meteorRenderer >>= SDL.destroyRenderer
  use meteorWindow >>= SDL.destroyWindow
  SDL.quit

dieE :: SDLErr -> IO ()
dieE e = putStrLn $ "ERROR: " <> show e

addMissile :: (Actor,Col) -> Vector SDL.Rect -> Vector SDL.Rect
addMissile p = V.cons newM
  where
    ppos l = p ^. _1 . actorRect . l
    newM = _Rect # (ppos rectX',ppos rectY',15,15)

moveP :: SDL.Keysym -> El ()
moveP k = case SDL.keysymKeycode k of
  SDL.SDLK_LEFT  -> posUpdate rectX' (subtract distance)
  SDL.SDLK_RIGHT -> posUpdate rectX' (+distance)
  SDL.SDLK_SPACE -> do
    p <- use meteorPlayer
    meteorMissiles %= addMissile p
  _ -> return ()
  where
    distance = 10
    posUpdate lns g = meteorPlayer . _1 . actorRect . lns %=
                      (\xV -> bnds screenWidth xV $ g xV)

    bnds sW o n = bool o n $ n > 0 && n < sW

updatePlayer :: SDL.Event -> El ()
updatePlayer (SDL.KeyboardEvent SDL.SDL_KEYDOWN _ _ _ _ kSym) = moveP kSym
updatePlayer (SDL.QuitEvent _ _) = gameover .= True
updatePlayer _ = return ()

updateActors :: Maybe SDL.Event -> El ()
updateActors = maybe (return ()) updatePlayer

checkGameOver :: El Bool
checkGameOver = do
  fin <- use gameover
  win <- use meteorMobs
  p   <- use meteorPlayer
  return $ or [ fin
              , 0 == Map.size win
              , notEmpty $ passedPlayer p win
              ]
  where
    notEmpty = (>0) . Map.size
    passedPlayer p =
      Map.filter (^. actorRect . rectY' . to (>= p ^. _1 . actorRect . rectY'))

mainLoop :: El ()
mainLoop = do
  renderWith >> handleInputs
  fin <- checkGameOver
  unless fin mainLoop
  where
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
    emptyBullets = V.empty

    mkMeteor (w,r) = MeteorS w r
                     getInitialPlayer
                     emptyBullets -- no missiles to start the game
                     getInitialMobs
                     False

main :: IO ()
main = do
  mS <- createMeteor
  case mS of
    Left e -> dieE e
    Right m -> runEl m (mainLoop >> quitApp)
               >>= either dieE (\_ -> putStrLn "You might have won...")
