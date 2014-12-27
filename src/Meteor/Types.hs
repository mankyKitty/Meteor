{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Meteor.Types where

import Prelude ()
import BasePrelude

import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Either (EitherT(..),runEitherT)

newtype El a = El
  { unEl :: EitherT SDLErr IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             )

data SDLErr
  = SDLInitError
  | WindowCreationError
  | SDLWinAndRndrCreationError
  | RenderError
  | RenderClearError
  | ColourSetError
  deriving Show

elGrande :: (Monad m, MonadIO m) => El a -> m (Either SDLErr a)
elGrande = liftIO . runEitherT . unEl
