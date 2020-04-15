module PDUp.Incident
    ( DateRange
    , dateRange
    , dateRangeSince
    , dateRangeUntil
    , Incident
    , incidentBegan
    , incidentResolved
    , sourceIncidents
    )
where

import RIO

import Conduit
import Control.Error.Util (hush)
import Data.Aeson (FromJSON(..))
import qualified Data.ByteString.Char8 as C8
import Data.Time.ISO8601 (formatISO8601)
import Network.HTTP.Paginate
import Network.HTTP.Retry
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import Network.HTTP.Types.Status (status200)
import PDUp.Token
import RIO.Time (UTCTime)

data DateRange = DateRange
    { dateRangeSince :: UTCTime
    , dateRangeUntil :: UTCTime
    }

dateRange :: UTCTime -> UTCTime -> Either String DateRange
dateRange since until
    | since > until = Left "Since cannot be greater than until"
    | otherwise = Right DateRange
        { dateRangeSince = since
        , dateRangeUntil = until
        }

data Incidents = Incidents
    { limit :: Natural
    , offset :: Natural
    , more :: Bool
    , incidents :: [Incident]
    -- ^ The reason we can't (easily) make a @Paginated a@ type
    --
    -- The key pointing at the underlying items is not generic; it depends on
    -- the resource in question. If we ever have more, and want such a
    -- generalized type, we'll probably have to use a type-level string:
    --
    -- @
    -- type Incidents = Paginated "incidents" Incident
    -- @
    --
    -- Deferred for now, since we only deal in one resource today.
    --
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

data Incident = Incident
    { incident_number :: Integer
    , created_at :: UTCTime
    , status :: Text
    , last_status_change_at :: UTCTime
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

incidentBegan :: Incident -> UTCTime
incidentBegan = created_at

incidentResolved :: Incident -> Maybe UTCTime
incidentResolved incident = do
    guard $ status incident == "resolved"
    pure $ last_status_change_at incident

sourceIncidents
    :: HasLogFunc env
    => Token
    -> DateRange
    -> ConduitT () [Incident] (RIO env) ()
sourceIncidents token range = do
    req <-
        parseRequest
        $ "https://api.pagerduty.com/incidents"
        <> "?sort_by=created_at"
        <> "&limit=500"
        <> "&offset=0"

    sourcePaginatedBy
            pdPagination
            (rateLimited $ httpJSONEither . setRequest token range)
            (setRequest token range req)
        .| iterMC (logDebug . displayShow)
        .| mapMC fromJSONExceptionThrow
        .| mapC incidents

pdPagination
    :: Request -> Response (Either JSONException Incidents) -> Maybe Request
pdPagination req resp = do
    guard $ getResponseStatus resp == status200
    incidents <- hush $ getResponseBody resp
    guard $ more incidents
    let nextOffset = offset incidents + limit incidents
        nextQs = [("offset", Just $ C8.pack $ show nextOffset)]
    pure $ updateRequestQueryString nextQs req

setRequest :: Token -> DateRange -> Request -> Request
setRequest token range = setFilters range . setAccept . setAuthorization token

setFilters :: DateRange -> Request -> Request
setFilters DateRange {..} = updateRequestQueryString
    [ ("since", Just $ C8.pack $ formatISO8601 dateRangeSince)
    , ("until", Just $ C8.pack $ formatISO8601 dateRangeUntil)
    ]

setAccept :: Request -> Request
setAccept =
    setRequestHeader hAccept ["application/vnd.pagerduty+json;version=2"]

setAuthorization :: Token -> Request -> Request
setAuthorization token = setRequestHeader
    hAuthorization
    ["Token token=" <> encodeUtf8 (unToken token)]

--------------------------------------------------------------------------------
-- Generic Network.HTTP stuff
--------------------------------------------------------------------------------

updateRequestQueryString
    :: [(ByteString, Maybe ByteString)] -> Request -> Request
updateRequestQueryString qs req = setRequestQueryString (keptQs <> qs) req
  where
    keys = map fst qs
    keptQs = filter ((`notElem` keys) . fst) $ getRequestQueryString req

-- | Re-creates the behavior of @'httpJSON'@, but deferred until later
--
-- Using @'httpJSON'@ directly will throw these exceptions too early, e.g.
-- before we can attempt retries on 429s. By using @'httpJSONEither'@ and then
-- mapping this over the post-retry result, it all works.
--
fromJSONExceptionThrow :: MonadIO m => Response (Either JSONException a) -> m a
fromJSONExceptionThrow = either throwIO pure . getResponseBody
