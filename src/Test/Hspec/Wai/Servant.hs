-- | Write tests in the style of Test.Hspec.Wai for your Servant APIs
--
-- See tests for usage examples
module Test.Hspec.Wai.Servant(
    module Test.Hspec.Wai.Servant.Client
  , module Test.Hspec.Wai.Servant.Assertions
  , getTestResponse
  , TestResponse ()
  -- * re-exports
  , module HspecWai) where

import           Test.Hspec.Wai.Servant.Assertions
import           Test.Hspec.Wai.Servant.Client
import           Test.Hspec.Wai.Servant.Types      (TestResponse, getTestResponse)

import           Test.Hspec.Wai                as HspecWai (WaiExpectation,
                                                            WaiSession, liftIO,
                                                            with)
