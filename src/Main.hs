{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Prelude ()
import BasePrelude hiding (left,right)

import qualified Graphics.UI.SDL as SDL

import Foreign.C.Types

import Control.Lens ((#),(^.),_1,_2,(&),(.~),(%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.State (gets,modify)
import Meteor.Types
import Meteor.SDLExtras

hgt :: CInt
hgt = 480

wdt :: CInt
wdt = 640

getRects :: (SDL.Rect,SDL.Rect)
getRects = ( _Rect # (wdt `div` 4, hgt `div` 4, wdt `div` 2, hgt `div` 2) 
           , _Rect # (30, 30, 30, 30)
           )

getColours :: (Col,Col)
getColours = ( _Col # (0x00,0xFF,0x00,0xFF)
             , _Col # (0xFF,0x00,0xFF,0x00)
             )

renderWith :: El ()
renderWith = do
  let cs = getColours
  rndrr <- gets _meteorRenderer
  (fence,player) <- gets _meteorRects
              
  -- set the background colour
  setColour rndrr 0x00 0x00 0x00 0xFF >> clearRender rndrr

  -- draw every rect to the screen
  dRect rndrr (fence, cs ^. _1)
  dRect rndrr (player, cs ^. _2)

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

moveP :: SDL.Keysym -> MeteorS -> MeteorS
moveP k s = case SDL.keysymKeycode k of
  SDL.SDLK_w -> upd (subtract distance)
  SDL.SDLK_s -> upd (+distance)
  SDL.SDLK_a -> lfr (subtract distance)
  SDL.SDLK_d -> lfr (+distance)
  _ -> s
  where
    distance = 5
    lfr :: (CInt -> CInt) -> MeteorS
    lfr f = s & meteorRects . _2 . rectX' %~ f
    upd :: (CInt -> CInt) -> MeteorS
    upd f = s & meteorRects . _2 . rectY' %~ f

updatePlayer :: SDL.Event -> MeteorS -> MeteorS
updatePlayer (SDL.KeyboardEvent _ _ _ _ _ kSym) m = moveP kSym m
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
  (w,r,_) <- mkWindowAndRenderer hgt wdt
  return (w,r)

createMeteor :: IO (Either SDLErr MeteorS)
createMeteor = do
  eM <- runEitherT initialise 
  return $ mkMeteor <$> eM
  where
    mkMeteor (w,r) = MeteorS w r getRects False
  
main :: IO ()
main = do
  mS <- createMeteor
  case mS of
    Left e -> dieE e
    Right m -> runEl m (mainLoop >> quitApp) >>= either dieE (\_ -> putStrLn "victory!")
