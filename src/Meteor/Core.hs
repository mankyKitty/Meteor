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

missileColour :: Col
missileColour = _Col # (0x00,0xFF,0xFF,0x00)
