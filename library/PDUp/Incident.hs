module PDUp.Incident
    ( Incident
    , fetchIncidents
    )
where

import RIO

import PDUp.Token
import RIO.Time (UTCTime)

data Incident

fetchIncidents :: Token -> UTCTime -> UTCTime -> RIO env [Incident]
fetchIncidents = undefined
