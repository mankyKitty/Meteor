{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Meteor.Types where

import           Control.Lens               (makeLenses, makePrisms, view)

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State        (MonadState, StateT (..))
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)

import           Data.Bool                  (bool)
import           Data.Word                  (Word8)
import           Graphics.UI.SDL            (Rect, Renderer, Window)

import           Data.Map                   (Map)
import           Data.Vector                (Vector)

data Col = Col
  { _colRed   :: Word8
  , _colGreen :: Word8
  , _colBlue  :: Word8
  , _colAlpha :: Word8
  } deriving Show
makeLenses ''Col
makePrisms ''Col

data ActorType
  = Player
  | NPC
  | Bullet
  deriving (Show,Eq)

data Actor = Actor
  { _actorRect :: Rect
  , _actorType :: ActorType
  } deriving (Show,Eq)
makeLenses ''Actor

type Player = Actor
type NPC = Actor

type ActorMap = Map Int Actor

data MeteorS = MeteorS
  { _meteorWindow   :: Window
  , _meteorRenderer :: Renderer
  , _meteorPlayer   :: (Player,Col)
  , _meteorMissiles :: Vector Rect
  , _meteorMobs     :: ActorMap
  , _gameover       :: Bool
  }
makeLenses ''MeteorS

class (MonadIO m, MonadError SDLErr m) => HasSDLErr m where
  decide  :: (a -> Bool) -> SDLErr -> IO a -> m a
  decide' :: (Eq n, Num n) => SDLErr -> IO n -> m ()

hasSDLErr
  :: ( MonadIO m
     , MonadError e m
     )
  => (a -> b)
  -> (a -> Bool)
  -> e
  -> IO a
  -> m b
hasSDLErr g f e a = liftIO a >>= hasE
  where
    hasE r = bool (return $ g r) (throwError e) $ f r

instance HasSDLErr El where
  decide  = hasSDLErr id
  decide' = hasSDLErr (const ()) (/= 0)

instance HasSDLErr (EitherT SDLErr IO) where
  decide  = hasSDLErr id
  decide' = hasSDLErr (const ()) (/= 0)

type Et a = EitherT SDLErr IO a

class HasRect a where
    getRect :: a -> Rect

instance HasRect Rect where
    getRect = id

instance HasRect Actor where
    getRect = view actorRect

newtype El a = El
  { unEl :: StateT MeteorS (EitherT SDLErr IO) a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadState MeteorS
             , MonadError SDLErr
             )

data SDLErr
  = SDLInitError
  | WindowCreateError
  | RendererCreateError
  | SDLWinAndRndrCreationError
  | RenderError
  | RenderClearError
  | ColourSetError
  deriving Show

runEl 
  :: ( Monad m
     , MonadIO m
     ) 
  => MeteorS 
  -> El a 
  -> m (Either SDLErr (a,MeteorS))
runEl meteorS = liftIO . runEitherT . flip runStateT meteorS . unEl
