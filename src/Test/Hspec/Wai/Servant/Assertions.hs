{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Assertions for use with the result of the generated client functions
module Test.Hspec.Wai.Servant.Assertions
  ( shouldRespondWith
  , shouldRespondWith_
  , succeed
  , dontSucceed
  ) where

import           Data.Functor                  (void)
import           GHC.Stack                     (HasCallStack)
import           Network.HTTP.Types.Status     (ok200)
import           Network.Wai.Test              (SResponse (..))
import qualified Test.Hspec.Wai                as W

import           Test.Hspec.Wai.Servant.Client (putExpectationFailure)
import           Test.Hspec.Wai.Servant.Types

-- | Like @shouldRespondWith@ from 'Test.Hspec.Wai', but ...
-- 1) operates on @WaiSession (TestResponse a)@ instead of @(WaiSession SResponse)@
-- 2) returns the response for later use
shouldRespondWith
  :: HasCallStack
  => W.WaiSession st (TestResponse st a)
  -> W.ResponseMatcher
  -> W.WaiSession st (TestResponse st a)
shouldRespondWith action matcher = do
  tresp@(TestResponse _ _ sresp) <- action
  pure sresp `W.shouldRespondWith` matcher
  pure tresp

-- | Like 'shouldRespondWith', but doesn't return the response
shouldRespondWith_
  :: HasCallStack
  => W.WaiSession st (TestResponse st a)
  -> W.ResponseMatcher
  -> W.WaiExpectation st
shouldRespondWith_ = (void .) . shouldRespondWith

-- | Checks if the provided @action@ returns 200. If so, attempts to decode
-- the response.
succeed :: HasCallStack => W.WaiSession st (TestResponse st a) -> W.WaiSession st a
succeed action = do
  tresp@(TestResponse _ req sres@(SResponse status _ _)) <- action
  if status == ok200
     then getTestResponse tresp
     else putExpectationFailure "" "status 200" sres req >> error "unreachable"

-- | Checks if the provided @action@ returns anything other than a 200. If so,
-- sally forth.
dontSucceed :: HasCallStack => W.WaiSession st (TestResponse st a) -> W.WaiSession st ()
dontSucceed action = do
  TestResponse _ req sres@(SResponse status _ _) <- action
  if status /= ok200
  then return ()
  else putExpectationFailure "" "anything but status 200" sres req
