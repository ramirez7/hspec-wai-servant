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
module Test.Hspec.Servant.Client
  ( client
  , HasTestClient
  ) where

import           Network.Wai.Test                (SResponse (..))
import           Test.Hspec.Wai

import           Control.Exception               (throwIO)
import           Data.ByteString.Char8           as BC
import qualified Data.CaseInsensitive            as CI
import           Data.Monoid                     ((<>))
import           Data.Proxy
import           GHC.TypeLits
import           Network.HTTP.Media.RenderHeader as HT
import qualified Network.HTTP.Types              as HT

import           Servant.API
import           Servant.Common.Req              (ServantError (..))

import           Test.Hspec.Servant.Types

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
decodeResponse ctProxy resp = liftIO $ either (throwIO . mkError) pure $ mimeUnrender ctProxy (simpleBody resp)
  where
    ct = contentType ctProxy
    mkError err = DecodeFailure err ct (simpleBody resp)

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
