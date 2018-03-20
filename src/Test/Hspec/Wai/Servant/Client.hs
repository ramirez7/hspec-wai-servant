{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Like 'Servant.Client', but instead generates a client suitable for use
-- on top of 'Test.Hspec.Wai'
--
-- Not every servant API combinator is implemented yet.
module Test.Hspec.Wai.Servant.Client
  ( client
  , HasTestClient
  ) where

import           Network.Wai.Test                                (SResponse (..))
import           Test.Hspec.Wai

import           Control.Exception                               (Exception)
import qualified Data.ByteString.Char8                           as BC
import qualified Data.CaseInsensitive                            as CI
import           Data.Monoid                                     ((<>))
import           Data.Proxy
import           Data.Typeable                                   (Typeable)
import           GHC.TypeLits
import qualified Network.HTTP.Media.MediaType                    as HT
import qualified Network.HTTP.Media.RenderHeader                 as HT
import qualified Network.HTTP.Types                              as HT
import           Servant.API
import           Servant.Checked.Exceptions.Internal.Envelope    (Envelope)
import           Servant.Checked.Exceptions.Internal.Servant.API (NoThrow,
                                                                  Throwing,
                                                                  ThrowingNonterminal,
                                                                  Throws)
import           Test.Hspec.Expectations                         (expectationFailure)

import           Test.Hspec.Wai.Servant.Types

-- | Works just like @client@ from 'Servant.Client', but the returned values are
-- @WaiSession (TestResponse a)@ instead of @ClientM a@
client :: HasTestClient api => Proxy api -> TestClient api
client p = testClientWithRoute p defReq

performTestRequest :: HT.Method -> TestRequest -> WaiSession SResponse
performTestRequest method TestRequest{..} = request method pathWithQuery testHeaders testBody
  where
    pathWithQuery = testPath <> HT.renderQuery True testQuery

performTestRequestCT :: (MimeUnrender ct a, ReflectMethod method) => Proxy ct -> Proxy method -> TestRequest -> WaiSession (TestResponse a)
performTestRequestCT ctP methodP req@TestRequest{..} =
  let method = reflectMethod methodP
      acceptCT = contentType ctP
      reqWithCt = req { testHeaders = ("accept", HT.renderHeader acceptCT) : testHeaders }
  in TestResponse (decodeResponse ctP) <$> performTestRequest method reqWithCt

-- | Will throw and fail the test if a fails to parse
decodeResponse :: MimeUnrender ctype a => Proxy ctype -> SResponse -> WaiSession a
decodeResponse ctProxy resp = liftIO $ either throwErr pure $ mimeUnrender ctProxy (simpleBody resp)
  where
    throwErr err = do
      liftIO $ expectationFailure $ unlines [ "Failed decoding response:"
                                            , show resp
                                            , "with error:"
                                            , show err
                                            ]
      error "unreachable"

data Err = DecodeError !String !HT.MediaType !BC.ByteString deriving (Show, Typeable)

instance Exception Err

-- | Type class to generate 'WaiSession'-based client handlers. Compare to
-- 'HasClient' from 'Servant.Client'
class HasTestClient api where
  type TestClient api :: *

  testClientWithRoute :: Proxy api -> TestRequest -> TestClient api

instance (HasTestClient a, HasTestClient b) => HasTestClient (a :<|> b) where
  type TestClient (a :<|> b) = TestClient a :<|> TestClient b

  testClientWithRoute Proxy req =
    testClientWithRoute (Proxy :: Proxy a) req :<|>
    testClientWithRoute (Proxy :: Proxy b) req

instance ( MimeUnrender ct a
         , ReflectMethod method
         , cts' ~ (ct ': cts)
         ) => HasTestClient (Verb method status cts' a) where
  type TestClient (Verb method status cts' a) = WaiSession (TestResponse a)

  testClientWithRoute Proxy req = performTestRequestCT ct method req
    where
      ct = Proxy :: Proxy ct
      method = Proxy :: Proxy method

instance (KnownSymbol capture, ToHttpApiData a, HasTestClient api)
      => HasTestClient (Capture capture a :> api) where

  type TestClient (Capture capture a :> api) =
    a -> TestClient api

  testClientWithRoute Proxy req val =
    testClientWithRoute api (appendToPath val req)
    where
      api = Proxy :: Proxy api


instance (KnownSymbol sym, ToHttpApiData a, HasTestClient api)
      => HasTestClient (QueryParam sym a :> api) where

  type TestClient (QueryParam sym a :> api) =
    Maybe a -> TestClient api

  testClientWithRoute Proxy req mparam =
    testClientWithRoute api (appendToQueryString qname mparam req)
    where
      api = Proxy :: Proxy api
      qname = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToHttpApiData a, HasTestClient api)
      => HasTestClient (QueryParams sym a :> api) where

  type TestClient (QueryParams sym a :> api) =
    [a] -> TestClient api

  testClientWithRoute Proxy req params =
    testClientWithRoute api (appendManyToQueryString qname params req)
    where
      api = Proxy :: Proxy api
      qname = symbolVal (Proxy :: Proxy sym)

instance (MimeRender ct a, HasTestClient api)
      => HasTestClient (ReqBody (ct ': cts) a :> api) where
  type TestClient (ReqBody (ct ': cts) a :> api) =
    a -> TestClient api

  testClientWithRoute Proxy req body =
    testClientWithRoute api (setReqBody ct body req)
    where
      api = Proxy :: Proxy api
      ct = Proxy :: Proxy ct

instance (KnownSymbol sym, ToHttpApiData a, HasTestClient api)
      => HasTestClient (Header sym a :> api) where

  type TestClient (Header sym a :> api) =
    Maybe a -> TestClient api

  testClientWithRoute Proxy req mheader = testClientWithRoute api reqWithHeader
    where
      api = Proxy :: Proxy api
      hname = CI.mk $ BC.pack $ symbolVal (Proxy :: Proxy sym)
      reqWithHeader = maybe req (\h -> appendHeader hname h req) mheader

instance (KnownSymbol path, HasTestClient api) => HasTestClient (path :> api) where
  type TestClient (path :> api) = TestClient api

  testClientWithRoute Proxy req = testClientWithRoute api (appendToPath path req)
    where
      api = Proxy :: Proxy api
      path = symbolVal (Proxy :: Proxy path)

-- servant-checked-exception instances

instance (HasTestClient (Throwing '[e] :> api)) => HasTestClient (Throws e :> api) where
  type TestClient (Throws e :> api) = TestClient (Throwing '[e] :> api)

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy (Throwing '[e] :> api)

instance (HasTestClient (Verb method status ctypes (Envelope es a))) =>
    HasTestClient (Throwing es :> Verb method status ctypes a) where

  type TestClient (Throwing es :> Verb method status ctypes a) =
    TestClient (Verb method status ctypes (Envelope es a))

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy (Verb method status ctypes (Envelope es a))

instance (HasTestClient (Verb method status ctypes (Envelope '[] a))) =>
    HasTestClient (NoThrow :> Verb method status ctypes a) where

  type TestClient (NoThrow :> Verb method status ctypes a) =
    TestClient (Verb method status ctypes (Envelope '[] a))

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy (Verb method status ctypes (Envelope '[] a))

instance (HasTestClient ((Throwing es :> api1) :<|> (Throwing es :> api2))) =>
    HasTestClient (Throwing es :> (api1 :<|> api2)) where

  type TestClient (Throwing es :> (api1 :<|> api2)) =
    TestClient ((Throwing es :> api1) :<|> (Throwing es :> api2))

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy ((Throwing es :> api1) :<|> (Throwing es :> api2))

instance (HasTestClient ((NoThrow :> api1) :<|> (NoThrow :> api2))) =>
    HasTestClient (NoThrow :> (api1 :<|> api2)) where

  type TestClient (NoThrow :> (api1 :<|> api2)) =
    TestClient ((NoThrow :> api1) :<|> (NoThrow :> api2))

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy ((NoThrow :> api1) :<|> (NoThrow :> api2))

instance (HasTestClient (ThrowingNonterminal (Throwing es :> api :> apis))) =>
    HasTestClient (Throwing es :> api :> apis) where

  type TestClient (Throwing es :> api :> apis) =
    TestClient (ThrowingNonterminal (Throwing es :> api :> apis))

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis))

instance (HasTestClient (api :> NoThrow :> apis)) =>
    HasTestClient (NoThrow :> api :> apis) where

  type TestClient (NoThrow :> api :> apis) =
    TestClient (api :> NoThrow :> apis)

  testClientWithRoute Proxy = testClientWithRoute api
    where
      api = Proxy :: Proxy (api :> NoThrow :> apis)
