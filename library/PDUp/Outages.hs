module PDUp.Outages
  ( Outages
  , traverseOutages_
  , outagesMinutes
  , emptyOutages
  , Outage
  , addOutageFromIncident
  )
where

import RIO

import PDUp.Incident
import qualified Rampart
import RIO.Time (UTCTime, diffUTCTime)

newtype Outages = Outages [Outage]

traverseOutages_ :: Applicative f => (Outage -> f a) -> Outages -> f ()
traverseOutages_ f (Outages outages) = traverse_ f outages

outagesMinutes :: Outages -> Integer
outagesMinutes (Outages outages) = sum $ map outageMinutes outages

emptyOutages :: Outages
emptyOutages = Outages []

data Outage = Outage
  { outageBegan :: UTCTime
  , outageResolved :: UTCTime
  }

instance Display Outage where
  display o@Outage {..} =
    displayShow outageBegan
      <> " to "
      <> displayShow outageResolved
      <> ", "
      <> displayShow (outageMinutes o)
      <> " minute(s)"

outageMinutes :: Outage -> Integer
outageMinutes Outage {..} = round $ diffUTCTime outageResolved outageBegan / 60

addOutageFromIncident :: UTCTime -> Outages -> Incident -> Outages
addOutageFromIncident now outages = addOutage outages . fromIncident now

-- | Add a new Outage, ensuring we don't double-count
--
-- Assuptions:
--
-- - We see each Outage in order by when it began
-- - We've accounted for overlap so far
--
-- Therefore:
--
-- - We only have to check for overlap against the last outage
-- - And we either combine, or append
--
-- TODO: is there a better structure for this, where last/snoc is as fast as
-- head/cons and we don't have to do the double-reversing?
--
addOutage :: Outages -> Outage -> Outages
addOutage (Outages outages) x = Outages $ reverse $ go $ reverse outages
 where
  go [] = [x]
  go (y : ys) = case outageRelation x y of
    Before -> [y, x] <> ys
    Concurrent -> mergeOutages x y : ys
    After -> [x, y] <> ys

data OutageRelation
  = Before
  | Concurrent
  | After

outageRelation :: Outage -> Outage -> OutageRelation
outageRelation a b = case relation of
  Rampart.Before -> Before
  Rampart.Meets -> Concurrent
  Rampart.Overlaps -> Concurrent
  Rampart.FinishedBy -> Concurrent
  Rampart.Contains -> Concurrent
  Rampart.Starts -> Concurrent
  Rampart.Equal -> Concurrent
  Rampart.StartedBy -> Concurrent
  Rampart.During -> Concurrent
  Rampart.Finishes -> Concurrent
  Rampart.OverlappedBy -> Concurrent
  Rampart.MetBy -> Concurrent
  Rampart.After -> After
 where
  relation = Rampart.relate
    (Rampart.toInterval (outageBegan a, outageResolved a))
    (Rampart.toInterval (outageBegan b, outageResolved b))

mergeOutages :: Outage -> Outage -> Outage
mergeOutages a b = Outage
  { outageBegan = min (outageBegan a) (outageBegan b)
  , outageResolved = max (outageResolved a) (outageResolved b)
  }

fromIncident :: UTCTime -> Incident -> Outage
fromIncident now incident = Outage
  { outageBegan = incidentBegan incident
  , outageResolved = fromMaybe now $ incidentResolved incident
  }
