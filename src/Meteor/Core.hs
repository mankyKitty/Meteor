{-# LANGUAGE RankNTypes #-}
module Meteor.Core where

import Prelude ()
import BasePrelude

import Control.Lens (Lens',(#),(^.),view)

import Foreign.C.Types (CInt(..))
import Meteor.Types
import Meteor.SDLExtras

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Graphics.UI.SDL (Rect)

screenHeight :: CInt
screenHeight = 480

screenWidth :: CInt
screenWidth = 640

getInitialPlayer :: (Player,Col)
getInitialPlayer = (playerStart,playerColour)

playerStart :: Player
playerStart = Actor
              (_Rect # (screenWidth `div` 2, screenHeight - 60, 30, 30))
              Player

playerColour :: Col
playerColour = _Col # (0xFF,0x00,0xFF,0x00)

mobColour :: Col
mobColour = _Col # (0xAA,0xFF,0xBB,0xFF)

missileColour :: Col
missileColour = _Col # (0x00,0xFF,0xFF,0x00)

getInitialMobs :: ActorMap
getInitialMobs = Map.fromList $ lst mobs
  where
    lst m = zip [1..length m] m
    mobs = [mob x | x <- [100,200..600]]
    mob buf = Actor (_Rect # (screenWidth - buf, 20, 25,25)) NPC

rectBnds :: Lens' Rect CInt -> Rect -> CInt
rectBnds cart a = sum [ a ^. cart
                      , a ^. rectW'
                      , a ^. rectH'
                      ]

rectInter :: Rect -> Rect -> Bool
rectInter a b = and [
  x1 a < x2 b,
  x2 a > x1 b,

  y1 a < y2 b,
  y2 a > y1 b
  ]
  where
    x1 = view rectX'
    y1 = view rectY'

    x2 = rectBnds rectX'
    y2 = rectBnds rectY'
    
hitsRegistered :: HasRect a => Vector Rect -> a -> Maybe Int
hitsRegistered bullets a = V.findIndex f bullets
    where
      mRect = getRect a
      f = rectInter mRect

getHits :: HasRect a => Map Int a -> Vector Rect -> Maybe ([Int],[Int])
getHits m b
  | b == mempty = Nothing
  | otherwise = mayHits hits
  where
    toTuple = Just . ((,) <$> Map.keys <*> Map.elems)
    mayHits h = bool Nothing (toTuple h) $ Map.size h > 0
    hits = Map.mapMaybe (hitsRegistered b) m
