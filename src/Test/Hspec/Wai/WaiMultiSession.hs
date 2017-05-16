{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | 'WaiSession' is limited because its only a 'Reader' of a single 'Application'
-- This is a pain if you want to test multiple 'Application's that call each other
-- 'WaiMultiSession' is very similar to 'WaiSession', but it is a 'Reader' of
-- multiple 'Application's, each of which is tagged at the type level with a 'Symbol'
--
-- This module actually has nothing to do with 'servant', but it is a pain to
-- use multiple 'Application's without 'hspec-wai-servant's auto client generation
-- so that's why it's in this library.
module Test.Hspec.Wai.WaiMultiSession where

import           Network.Wai             (Application)
import           Network.Wai.Test        (runSession)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Wai.Internal (WaiSession (..))

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (ReaderT (..))
import           Data.Proxy
import qualified GHC.Exts                as GHCX
import           GHC.TypeLits

-- | Run a given 'WaiSession' with the 'Application' tagged with @s@ in the
-- 'WaiMultiSession'
--
-- NOTE: 'ClientState' is NOT threaded through! It is just a wrapper around
-- Cookies, so if you aren't using Cookies, you're good.
-- FIXME: If @wai-extra@ exported 'ClientState', this would be able to be fixed.
sendWaiSession :: forall (s :: Symbol) (tags :: [Symbol]) a
                . (GetApplication s tags)
               => Proxy s
               -> Proxy tags
               -> WaiSession a
               -> WaiMultiSession tags a
sendWaiSession p _ (WaiSession session) =
  WaiMultiSession $ ReaderT $ \ma ->
    runSession session (getApplication p ma)

newtype WaiMultiSession (tags :: [Symbol]) a =
  WaiMultiSession { unWaiMultiSession :: (ReaderT (MultiApplication tags) IO a) }
  deriving (Functor, Applicative, Monad, MonadIO)

runWaiMultiSession :: WaiMultiSession tags a -> MultiApplication tags -> IO a
runWaiMultiSession session app = runReaderT (unWaiMultiSession session) app

type WaiMultiExpectation tags = WaiMultiSession tags ()

instance Example (WaiMultiExpectation tags) where
  type Arg (WaiMultiExpectation tags) = MultiApplication tags
  evaluateExample e p action = evaluateExample (action $ runWaiMultiSession e) p ($ ())

data MultiApplication (tags :: [Symbol]) where
  OneApp :: Application -> MultiApplication '[s]
  ManyApps :: Application -> MultiApplication xs -> MultiApplication (s ': xs)

class GetApplication (s :: Symbol) (tags :: [Symbol]) where
  getApplication :: Proxy s -> MultiApplication tags -> Application

instance forall (s :: Symbol). GetApplication s '[s] where
  getApplication _ (ManyApps _ _) = error "impossible"
  getApplication _ (OneApp app)   = app

instance {-# OVERLAPPABLE #-} forall (s :: Symbol) (xs :: [Symbol]). GetApplication s (s ': xs) where
  getApplication _ (ManyApps app _) = app
  getApplication _ (OneApp _)       = error "impossible"

instance {-# OVERLAPPABLE #-} forall (s :: Symbol) (xs :: [Symbol]) (x :: Symbol)
        . (GetApplication s xs)
       => GetApplication s (x ': xs) where
  getApplication p (ManyApps _ xs) = getApplication p xs
  getApplication _ (OneApp _)      = error "impossible"

instance KnownSymbol s => Show (MultiApplication '[s]) where
  show (OneApp _)     = show $ symbolVal (Proxy :: Proxy s)
  show (ManyApps _ _) = error "impossible"

instance {-# OVERLAPPABLE #-} forall (s :: Symbol) (xs :: [Symbol])
        . (KnownSymbol s, Show (MultiApplication xs))
       => Show (MultiApplication (s ': xs)) where
  show (ManyApps _ xs) = show (symbolVal (Proxy :: Proxy s)) ++ " : " ++ show xs
  show (OneApp _) = error "impossible"

instance forall (s :: Symbol). GHCX.IsList (MultiApplication '[s]) where
  type Item (MultiApplication '[s]) = Application

  fromList [app] = OneApp app
  fromList (_ : _ : _) = error "malformed (too long) MultiApplication OverloadedList!"
  fromList [] = error "malformed (too short) MultiApplication OverloadedList!"

  toList (OneApp app)   = [app]
  toList (ManyApps _ _) = error "impossible"

instance {-# OVERLAPPABLE #-} forall (s :: Symbol) (xs :: [Symbol])
        . ( GHCX.IsList (MultiApplication xs)
          , GHCX.Item (MultiApplication xs) ~ Application)
       => GHCX.IsList (MultiApplication (s ': xs)) where
  type Item (MultiApplication (s ': xs)) = Application

  fromList (app : xs) = ManyApps app (GHCX.fromList xs :: MultiApplication xs)
  fromList [] = error "malformed (too short) MultiApplication OverloadedList!"

  toList (ManyApps app xs) = app : GHCX.toList xs
  toList _                 = error "impossible"
