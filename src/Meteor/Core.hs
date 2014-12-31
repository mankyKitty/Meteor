module Meteor.Core where

import Prelude ()
import BasePrelude

import Control.Lens ((#))

import Foreign.C.Types (CInt(..))

import Meteor.Types
import Meteor.SDLExtras

import Graphics.UI.SDL (Rect)

screenHeight :: CInt
screenHeight = 480

screenWidth :: CInt
screenWidth = 640

getInitialPlayer :: (Rect,Col)
getInitialPlayer = (playerStart,playerColour)

playerStart :: Rect 
playerStart = _Rect # (screenWidth `div` 2, screenHeight - 60, 30, 30)

playerColour :: Col
playerColour = _Col # (0xFF,0x00,0xFF,0x00)

mobColour :: Col
mobColour = _Col # (0xAA,0xFF,0xBB,0xFF)

missileColour :: Col
missileColour = _Col # (0x00,0xFF,0xFF,0x00)

getInitialMobs :: [Rect]
getInitialMobs = [mob x | x <- [100,200..600]]
  where
    mob buf = _Rect # (screenWidth - buf, 20, 25,25)

checkForIntersect :: [Rect] -> Rect -> IO Bool
checkForIntersect mss mob =
  (not . getAny) <$> foldM inter' (Any False) mss
  where
    inter' :: Any -> Rect -> IO Any
    inter' hit ms = (mappend hit . Any) <$> rectIntersect mob ms

hitMobs :: [Rect] -> [Rect] -> IO [Rect]
hitMobs mbs [] = return mbs
hitMobs mbs mss = filterM (checkForIntersect mss) mbs
