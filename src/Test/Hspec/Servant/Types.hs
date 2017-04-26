{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.Hspec.Servant.Types where

import           Network.Wai.Test                (SResponse (..))
import           Test.Hspec.Wai

import           Data.ByteString                 as B
import           Data.ByteString.Char8           as BC
import           Data.ByteString.Conversion.To   (toByteString')
import           Data.ByteString.Lazy            as BL
import           Data.Monoid                     ((<>))
import           Data.Proxy                      (Proxy (..))
import           Network.HTTP.Media.RenderHeader as HT
import qualified Network.HTTP.Types              as HT
import           Servant.API                     (MimeRender (..),
                                                  ToHttpApiData (..),
                                                  contentType)

data TestRequest = TestRequest
  { testPath    :: B.ByteString
  , testQuery   :: HT.Query
  , testHeaders :: [HT.Header]
  , testBody    :: BL.ByteString
  }

defReq :: TestRequest
defReq = TestRequest mempty mempty mempty mempty

appendToPath :: ToHttpApiData a => a -> TestRequest -> TestRequest
appendToPath a req = req { testPath = testPath req <> "/" <> (toByteString' $ toUrlPiece a) }

appendHeader :: ToHttpApiData a => HT.HeaderName -> a -> TestRequest -> TestRequest
appendHeader hn a req = req { testHeaders = (hn, toHeader a) : testHeaders req }

appendToQueryString :: ToHttpApiData a => String -> Maybe a -> TestRequest -> TestRequest
appendToQueryString k v req = req { testQuery = (BC.pack k, toByteString' . toQueryParam <$> v) : testQuery req }

setReqBody :: (MimeRender ct a) => Proxy ct -> a -> TestRequest -> TestRequest
setReqBody ctP a req = req { testBody = mimeRender ctP a, testHeaders = ("content-type", HT.renderHeader (contentType ctP)) : testHeaders req }

-- | A raw SResponse along with a function to decode @a@
data TestResponse a = TestResponse (SResponse -> WaiSession a) SResponse

getTestResponse :: TestResponse a -> WaiSession a
getTestResponse (TestResponse k sresp) = k sresp
