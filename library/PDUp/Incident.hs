{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module PDUp.Incident
  ( Incident
  , TeamId(..)
  , ServiceId(..)
  , Urgency(..)
  , incidentBegan
  , incidentResolved
  , incidentUrgency
  , incidentSummary
  , incidentTeamIds
  , incidentServiceId
  , sourceIncidents
  )
where

import RIO hiding (id)

import Conduit
import Control.Error.Util (hush)
import Data.Aeson (FromJSON(..), withText)
import qualified Data.ByteString.Char8 as BS8
import Data.Time.ISO8601 (formatISO8601)
import Network.HTTP.Paginate
import Network.HTTP.Retry
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import Network.HTTP.Types.Status (status200)
import PDUp.DateRange
import PDUp.Token
import qualified RIO.NonEmpty as NE
import RIO.Text (unpack)
import qualified RIO.Text as T
import RIO.Time (UTCTime)

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
  { teams :: [Team]
  , service :: Service
  , incident_number :: Integer
  , created_at :: UTCTime
  , status :: Text
  , last_status_change_at :: UTCTime
  , urgency :: Urgency
  , summary :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype Team = Team
  { id :: TeamId
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype TeamId = TeamId
  { unTeamId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype FromJSON

teamId :: Team -> TeamId
teamId Team { id } = id

data Service = Service
  { id :: ServiceId
  , summary :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype ServiceId = ServiceId
  { unServiceId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype FromJSON

serviceId :: Service -> ServiceId
serviceId Service { id } = id

serviceSummary :: Service -> Maybe Text
serviceSummary Service { summary } = summary

data Urgency = Low | High
  deriving stock (Eq, Show)

instance FromJSON Urgency where
  parseJSON = withText "Urgency" $ \case
    "low" -> pure Low
    "high" -> pure High
    x -> fail $ "Unexpected urgency: " <> show x

incidentBegan :: Incident -> UTCTime
incidentBegan = created_at

incidentResolved :: Incident -> Maybe UTCTime
incidentResolved incident = do
  guard $ status incident == "resolved"
  pure $ last_status_change_at incident

incidentUrgency :: Incident -> Urgency
incidentUrgency = urgency

incidentSummary :: Incident -> Text
incidentSummary Incident { service, summary } = prefix <> summary
  where prefix = maybe "" (<> ": ") $ serviceSummary service

incidentTeamIds :: Incident -> [TeamId]
incidentTeamIds = map teamId . teams

incidentServiceId :: Incident -> ServiceId
incidentServiceId = serviceId . service

sourceIncidents
  :: HasLogFunc env
  => Maybe (NonEmpty TeamId)
  -> Token
  -> DateRange
  -> ConduitT () [Incident] (RIO env) ()
sourceIncidents mTeamIds token range = do
  req <-
    parseRequest
    $ "https://api.pagerduty.com/incidents"
    <> "?sort_by=created_at"
    <> "&limit=500"
    <> "&offset=0"
    <> maybe "" (("&" <>) . toTeamIdsParam) mTeamIds

  sourcePaginatedBy
      pdPagination
      (rateLimited $ httpJSONEither . setRequest token range)
      (setRequest token range req)
    .| iterMC (logDebug . displayShow)
    .| mapMC fromJSONExceptionThrow
    .| mapC incidents

 where
  toTeamIdsParam =
    unpack
      . ("team_ids%5B%5D=" <>)
      . T.intercalate "%2C"
      . map unTeamId
      . NE.toList

pdPagination
  :: Request -> Response (Either JSONException Incidents) -> Maybe Request
pdPagination req resp = do
  guard $ getResponseStatus resp == status200
  incidents <- hush $ getResponseBody resp
  guard $ more incidents
  let
    nextOffset = offset incidents + limit incidents
    nextQs = [("offset", Just $ BS8.pack $ show nextOffset)]
  pure $ updateRequestQueryString nextQs req

setRequest :: Token -> DateRange -> Request -> Request
setRequest token range = setFilters range . setAccept . setAuthorization token

setFilters :: DateRange -> Request -> Request
setFilters range = updateRequestQueryString
  [ ("since", Just $ BS8.pack $ formatISO8601 $ dateRangeSince range)
  , ("until", Just $ BS8.pack $ formatISO8601 $ dateRangeUntil range)
  ]

setAccept :: Request -> Request
setAccept =
  setRequestHeader hAccept ["application/vnd.pagerduty+json;version=2"]

setAuthorization :: Token -> Request -> Request
setAuthorization token =
  setRequestHeader hAuthorization ["Token token=" <> encodeUtf8 (unToken token)]

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
