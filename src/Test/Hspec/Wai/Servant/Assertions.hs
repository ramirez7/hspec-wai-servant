{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Assertions for use with the result of the generated client functions
module Test.Hspec.Wai.Servant.Assertions
  ( shouldRespondWith
  , shouldRespondWith_
  , succeed
  ) where

import           Data.Functor                 (void)
import           GHC.Stack                    (HasCallStack)
import qualified Test.Hspec.Wai               as W

import           Test.Hspec.Wai.Servant.Types

-- | Like @shouldRespondWith@ from 'Test.Hspec.Wai', but ...
-- 1) operates on @WaiSession (TestResponse a)@ instead of @(WaiSession SResponse)@
-- 2) returns the response for later use
shouldRespondWith :: HasCallStack => W.WaiSession (TestResponse a) -> W.ResponseMatcher -> W.WaiSession (TestResponse a)
shouldRespondWith action matcher = do
  tresp@(TestResponse _ sresp) <- action
  pure sresp `W.shouldRespondWith` matcher
  pure tresp

-- | Like 'shouldRespondWith', but doesn't return the response
shouldRespondWith_ :: HasCallStack => W.WaiSession (TestResponse a) -> W.ResponseMatcher -> W.WaiExpectation
shouldRespondWith_ = (void .) . shouldRespondWith

-- | Checks if the provided @action@ returns 200. If so, attempts to decode
-- the response
succeed :: HasCallStack => W.WaiSession (TestResponse a) -> W.WaiSession a
succeed action = do
  tresp <- action `shouldRespondWith` 200
  getTestResponse tresp
