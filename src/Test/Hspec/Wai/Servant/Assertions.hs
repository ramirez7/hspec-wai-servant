{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Assertions for use with the result of the generated client functions
module Test.Hspec.Wai.Servant.Assertions
  ( MonadHspecWai (..)
  , shouldRespondWith_
  , succeed
  ) where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Foldable                  (for_)
import           Data.Functor                   (void)
import           GHC.Stack                      (HasCallStack)
import           Test.Hspec                     (expectationFailure)
import qualified Test.Hspec.Wai                 as W
import qualified Test.Hspec.Wai.Matcher         as W

import           Test.Hspec.Wai.Servant.Types
import           Test.Hspec.Wai.WaiMultiSession

class Monad m => MonadHspecWai m where
  -- | Like @shouldRespondWith@ from 'Test.Hspec.Wai', but ...
  -- 1) operates on @m (TestResponse a)@ instead of @(WaiSession SResponse)@
  -- 2) returns the response for later use
  shouldRespondWith :: HasCallStack => m (TestResponse a) -> W.ResponseMatcher -> m (TestResponse a)

instance MonadHspecWai W.WaiSession where
  shouldRespondWith action matcher = do
    tresp@(TestResponse _ sresp) <- action
    pure sresp `W.shouldRespondWith` matcher
    pure tresp

instance MonadHspecWai (WaiMultiSession tags) where
  shouldRespondWith action matcher = do
    tresp@(TestResponse _ sresp) <- action
    for_ (W.match sresp matcher) (liftIO . expectationFailure)
    pure tresp


-- | Like 'shouldRespondWith', but doesn't return the response
shouldRespondWith_ :: (MonadHspecWai m, HasCallStack) => m (TestResponse a) -> W.ResponseMatcher -> m ()
shouldRespondWith_ = (void .) . shouldRespondWith

-- | Checks if the provided @action@ returns 200. If so, attempts to decode
-- the response, throwing on failure
succeed :: (HasCallStack, MonadIO m, MonadHspecWai m) => m (TestResponse a) -> m a
succeed action = do
  tresp <- action `shouldRespondWith` 200
  getTestResponse tresp
