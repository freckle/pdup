module PDUp.Outages
  ( Outages(..)
  , outagesMinutes
  , emptyOutages
  , Outage(..)
  , outageMinutes
  , addOutageFromIncident
  , addOutage
  )
where

import RIO

import PDUp.Incident
import Rampart.Simple
import RIO.Time (UTCTime, diffUTCTime)

newtype Outages = Outages { unOutages :: [Outage] }

outagesMinutes :: Outages -> Integer
outagesMinutes (Outages outages) = sum $ map outageMinutes outages

emptyOutages :: Outages
emptyOutages = Outages []

data Outage = Outage
  { outageBegan :: UTCTime
  , outageResolved :: UTCTime
  , outageSummaries :: [Text]
  }

-- instance Display Outage where
--   display o@Outage {..} =
--     displayShow outageBegan
--       <> " to "
--       <> displayShow outageResolved
--       <> ", "
--       <> displayShow (outageMinutes o)
--       <> " minute(s)"

outageMinutes :: Outage -> Integer
outageMinutes Outage {..} = round $ diffUTCTime outageResolved outageBegan / 60

addOutageFromIncident :: UTCTime -> Outages -> Incident -> Outages
addOutageFromIncident now outages = addOutage outages . fromIncident now

-- | Add a new Outage, ensuring we don't double-count
--
-- Assumptions:
--
-- - We see each Outage in order by when it began
-- - We've accounted for overlap so far
--
-- Therefore:
--
-- - We only have to check for overlap against the last outage
-- - And we either combine, or prepend
--
addOutage :: Outages -> Outage -> Outages
addOutage (Outages outages) x = Outages $ go outages
 where
  go [] = [x]
  go (y : ys) = case outageRelation x y of
    Before -> [y, x] <> ys
    Concurrent -> mergeOutages x y : ys
    After -> [x, y] <> ys

outageRelation :: Outage -> Outage -> Relation
outageRelation = relate `on` outageInterval

outageInterval :: Outage -> Interval UTCTime
outageInterval x = toInterval (outageBegan x, outageResolved x)

mergeOutages :: Outage -> Outage -> Outage
mergeOutages a b = Outage
  { outageBegan = min (outageBegan a) (outageBegan b)
  , outageResolved = max (outageResolved a) (outageResolved b)
  , outageSummaries = outageSummaries a <> outageSummaries b
  }

fromIncident :: UTCTime -> Incident -> Outage
fromIncident now incident = Outage
  { outageBegan = incidentBegan incident
  , outageResolved = fromMaybe now $ incidentResolved incident
  , outageSummaries = [incidentSummary incident]
  }
