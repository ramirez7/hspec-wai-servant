{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ClientSpec where

import           Test.Hspec
import           Test.Hspec.Wai.Servant

import           Control.Monad.Except   (throwError)
import           Data.Aeson             as A
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.ByteString        as B
import           Data.Maybe             (maybeToList)
import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as T

import           Network.Wai            (Application)
import           Servant.API
import           Servant.Server         (Server, err400, err500, serve)


-- | A type with a bad ToJSON instance.
-- This is here to check the decoding error message
-- by hand (repl).
newtype BadValue = BadValue Bool deriving (Show, Eq)

instance ToJSON BadValue where
  toJSON (BadValue _) = toJSON (1 :: Int)

instance FromJSON BadValue where
  parseJSON (A.Bool b) = return $ BadValue b
  parseJSON v          = typeMismatch "BadValue" v

spec :: Spec
spec =
  with (pure app) $
    describe "Test.Hspec.Servant.Client" $ do
      it "should do Status Code Checks" $ do
        idGet 1 `shouldRespondWith_` 200
        idPut 2 `shouldRespondWith_` 200
        idDelete 3 `shouldRespondWith_` 200
        idPost 4 `shouldRespondWith_` 200
        qparamMaybeToList (Just 5) `shouldRespondWith_` 200
        take' 2 [1,2,3,4,5] `shouldRespondWith_` 200
        reqbodyLength "abc123" `shouldRespondWith_` 200
        someHeaderLength (Just "xyz987") `shouldRespondWith_` 200
        _400s `shouldRespondWith_` 400
        _500s `shouldRespondWith_` 500
        dontSucceed _500s


      it "should do Response Checks" $ do
        succeed (idGet 1) >>= liftIO . (`shouldBe` 1)
        succeed (idPut 2) >>= liftIO . (`shouldBe` 2)
        succeed (idDelete 3) >>= liftIO . (`shouldBe` 3)
        succeed (idPost 4) >>= liftIO . (`shouldBe` 4)
        succeed (qparamMaybeToList (Just 5)) >>= liftIO . (`shouldBe` [5])
        succeed (take' 2 [1,2,3,4,5]) >>= liftIO . (`shouldBe` [1,2])
        succeed (reqbodyLength "abc123") >>= liftIO . (`shouldBe` 6)
        succeed (someHeaderLength (Just "xyz987")) >>= liftIO . (`shouldBe` 6)
        --succeed decodingError >>= liftIO . (`shouldBe` (BadValue True))

type API =
       "api" :> "identity" :> Capture "arg" Int :> Get '[JSON] Int
  :<|> "api" :> "identity" :> Capture "arg" Int :> Put '[JSON] Int
  :<|> "api" :> "identity" :> Capture "arg" Int :> Delete '[JSON] Int
  :<|> "api" :> "identity" :> Capture "arg" Int :> Post '[JSON] Int
  :<|> "api" :> "maybeToList" :> QueryParam "qparam" Int :> Get '[JSON] [Int]
  :<|> "api" :> "take" :> Capture "arg" Int :> QueryParams "qparam" Int :> Get '[JSON] [Int]
  :<|> "api" :> "postBodyLength" :> ReqBody '[OctetStream] B.ByteString :> Post '[JSON] Int
  :<|> "api" :> "someHeaderLength" :> Header "whatever" T.Text :> Get '[JSON] Int
  :<|> "api" :> "_400s" :> Get '[JSON] ()
  :<|> "api" :> "_500s" :> Get '[JSON] ()
  :<|> "api" :> "decodingError" :> Get '[JSON] BadValue

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = pure
    :<|> pure
    :<|> pure
    :<|> pure
    :<|> pure . maybeToList
    :<|> (pure .) . take
    :<|> pure . B.length
    :<|> pure . maybe 0 T.length
    :<|> throwError err400
    :<|> throwError err500
    :<|> pure (BadValue True)

idGet :: Int -> WaiSession () (TestResponse () Int)
idPut :: Int -> WaiSession () (TestResponse () Int)
idDelete :: Int -> WaiSession () (TestResponse () Int)
idPost :: Int -> WaiSession () (TestResponse () Int)
qparamMaybeToList :: Maybe Int -> WaiSession () (TestResponse () [Int])
take' :: Int -> [Int] -> WaiSession () (TestResponse () [Int])
reqbodyLength :: B.ByteString -> WaiSession () (TestResponse () Int)
someHeaderLength :: Maybe T.Text -> WaiSession () (TestResponse () Int)
_400s :: WaiSession () (TestResponse () ())
_500s :: WaiSession () (TestResponse () ())
decodingError :: WaiSession () (TestResponse () BadValue)

(      idGet
  :<|> idPut
  :<|> idDelete
  :<|> idPost
  :<|> qparamMaybeToList
  :<|> take'
  :<|> reqbodyLength
  :<|> someHeaderLength
  :<|> _400s
  :<|> _500s
  :<|> decodingError
  ) = client api
