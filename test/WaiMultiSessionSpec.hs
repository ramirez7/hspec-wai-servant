{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | These tests are examples of how to do integration tests between multiple
-- distinct Wai-based applications in hspec-wai form (no need to spin up multiple
-- concurrent servers on ports locally or test against a deployed environment)
module WaiMultiSessionSpec where

import           Test.Hspec
import           Test.Hspec.Wai.Internal        (runWaiSession)
import           Test.Hspec.Wai.Servant

import           Test.Hspec.Wai.WaiMultiSession

import           Control.Monad.IO.Class         (liftIO)
import           Data.Functor.Identity          (Identity (..))
import           Data.IORef
import           Data.Proxy                     (Proxy (..))

import           Network.Wai                    (Application)
import           Servant.API
import           Servant.Server                 (Server, serve)

{-
The API call graph for these tests looks like this
(arrow direction is dataflow, so X -> Y means Y calls X)

      -- /api/for_b ->
    A <- /api/for_a -- B
    |                  |
/api/from_b        /api/from_a
    |                  |
    -----> SumABC <-----
             |
         /api/sum/:c
             |
             v

The for_* APIs return the value of an IORef
The from_* APIs return the value from the other service
The SumABC API calls the two from_* APIs and adds their values with the provided :c
-}

spec :: Spec
spec = do
  forA <- runIO $ newIORef 0
  forB <- runIO $ newIORef 0

  let multiProxy = Proxy :: Proxy '["a", "b", "sum"]
  let sendA = sendWaiSession (Proxy :: Proxy "a") multiProxy
  let sendB = sendWaiSession (Proxy :: Proxy "b") multiProxy
  let sendSum = sendWaiSession (Proxy :: Proxy "sum") multiProxy

  with (pure (wireUp forA forB)) $ do
    describe "Test.Hspec.Wai.WaiMultiSession" $ do
      it "should dispatch to the specified Application" $ do
        liftIO $ writeIORef forA 1
        liftIO $ writeIORef forB 2
        (succeed $ sendB $ callForA) >>= liftIO . (`shouldBe` 1)
        (succeed $ sendA $ callForB) >>= liftIO . (`shouldBe` 2)
        (succeed $ sendSum $ callSumABC 3) >>= liftIO . (`shouldBe` 6)

        sendB (callForB) `shouldRespondWith_` 404
        sendA (callForA) `shouldRespondWith_` 404

-- | @RecursiveDo@ + 'Identity' can be used to easily wire up cyclical 'Application'
-- dependency graphs
wireUp :: IORef Int -> IORef Int -> MultiApplication '["a", "b", "sum"]
wireUp forA forB = runIdentity $ do
  rec appA <- Identity $ mkAppA appB forB
      appB <- Identity $ mkAppB appA forA
  let appSum = mkAppSumABC appA appB
  let (multiApp :: MultiApplication '["a", "b", "sum"]) = [appA, appB, appSum]
  Identity multiApp

type A = "api" :> "from_b" :> Get '[JSON] Int :<|> "api" :> "for_b" :> Get '[JSON] Int
apiA :: Proxy A
apiA = Proxy

callFromB :: WaiSession (TestResponse Int)
callForB :: WaiSession (TestResponse Int)
(callFromB :<|> callForB) = client apiA

serverA :: Application -> IORef Int -> Server A
serverA app forB = liftIO (runWaiSession (succeed callForA) app) :<|> liftIO (readIORef forB)

mkAppA :: Application -> IORef Int -> Application
mkAppA app forB = serve apiA $ serverA app forB

type B = "api" :> "from_a" :> Get '[JSON] Int :<|> "api" :> "for_a" :> Get '[JSON] Int
apiB :: Proxy B
apiB = Proxy

callFromA :: WaiSession (TestResponse Int)
callForA :: WaiSession (TestResponse Int)
(callFromA :<|> callForA) = client apiB

serverB :: Application -> IORef Int -> Server B
serverB app forA = liftIO (runWaiSession (succeed callForB) app) :<|> liftIO (readIORef forA)

mkAppB :: Application -> IORef Int -> Application
mkAppB app forA = serve apiB $ serverB app forA

type SumABC = "api" :> "sum" :> Capture "c" Int :> Get '[JSON] Int

apiSumABC :: Proxy SumABC
apiSumABC = Proxy

callSumABC :: Int -> WaiSession (TestResponse Int)
callSumABC = client apiSumABC

serverSumABC :: Application -> Application -> Server SumABC
serverSumABC appA appB = \c -> liftIO $ do
  a <- runWaiSession (succeed callFromB) appA
  b <- runWaiSession (succeed callFromA) appB
  pure $ a + b + c

mkAppSumABC :: Application -> Application -> Application
mkAppSumABC appA appB = serve apiSumABC $ serverSumABC appA appB
