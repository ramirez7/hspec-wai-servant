-- | Write tests in the style of Test.Hspec.Wai for your Servant APIs
--
-- See tests for usage examples
module Test.Hspec.Servant(
    module Test.Hspec.Servant.Client
  , module Test.Hspec.Servant.Assertions
  , getTestResponse
  , TestResponse ()
  -- * re-exports
  , module HspecWai) where

import           Test.Hspec.Servant.Assertions
import           Test.Hspec.Servant.Client
import           Test.Hspec.Servant.Types      (TestResponse, getTestResponse)

import           Test.Hspec.Wai                as HspecWai (WaiExpectation,
                                                            WaiSession, liftIO,
                                                            with)
