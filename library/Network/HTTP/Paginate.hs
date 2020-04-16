module Network.HTTP.Paginate
  ( sourcePaginatedLink
  , sourcePaginatedBy
  )
where

import Prelude

import Conduit
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Link hiding (linkHeader)
import Network.HTTP.Simple

sourcePaginatedLink
  :: MonadIO m
  => (Request -> IO (Response body)) -- ^ Run one request
  -> Request -- ^ Initial request
  -> ConduitT () (Response body) m ()
sourcePaginatedLink = sourcePaginatedBy linkHeader

sourcePaginatedBy
  :: MonadIO m
  => (Request -> Response body -> Maybe Request)
  -- ^ How to get the next page
  -> (Request -> IO (Response body))
  -- ^ How to run one request
  -> Request
  -- ^ The previous request
  -> ConduitT () (Response body) m ()
  -- ^ A source that yields each page's response
sourcePaginatedBy mNextRequest runRequest req = do
  resp <- liftIO $ runRequest req
  yield resp
  traverse_ (sourcePaginatedBy mNextRequest runRequest) $ mNextRequest req resp

linkHeader :: Request -> Response body -> Maybe Request
linkHeader _req resp = do
  header <- listToMaybe $ getResponseHeader "Link" resp
  links <- parseLinkHeader $ decodeUtf8 header
  uri <- href <$> find (((Rel, "next") `elem`) . linkParams) links
  parseRequest $ show uri
