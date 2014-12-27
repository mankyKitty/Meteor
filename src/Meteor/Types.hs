{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Meteor.Types where

import Prelude ()
import BasePrelude

import Control.Lens (makeLenses,makePrisms)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Either (EitherT(..),runEitherT)
import Control.Monad.State (StateT(..),MonadState)
import Control.Monad.Except (MonadError,throwError)
import Graphics.UI.SDL (Window,Renderer,Rect)

data MeteorS = MeteorS
  { _meteorWindow   :: Window
  , _meteorRenderer :: Renderer
  , _meteorRects    :: [Rect]
  }
makeLenses ''MeteorS
makePrisms ''MeteorS

class (MonadIO m, MonadError SDLErr m) => HasIOErr m where
  decide :: (a -> Bool) -> SDLErr -> IO a -> m a
  decide' :: (Eq n, Num n) => SDLErr -> IO n -> m ()

hasIOErr :: (MonadIO m, MonadError e m) => (a -> b) -> (a -> Bool) -> e -> IO a -> m b
hasIOErr g f e a = liftIO a >>= \r -> bool (return $ g r) (throwError e) $ f r

instance HasIOErr El where
  decide  = hasIOErr id
  decide' = hasIOErr (const ()) (/= 0)

instance HasIOErr (EitherT SDLErr IO) where
  decide  = hasIOErr id
  decide' = hasIOErr (const ()) (/= 0)
  
type Et a = EitherT SDLErr IO a

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
  | WindowCreationError
  | SDLWinAndRndrCreationError
  | RenderError
  | RenderClearError
  | ColourSetError
  deriving Show

runEl :: (Monad m, MonadIO m) => MeteorS -> El a -> m (Either SDLErr (a,MeteorS))
runEl meteorS = liftIO . runEitherT . flip runStateT meteorS . unEl
