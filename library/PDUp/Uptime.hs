module PDUp.Uptime
    ( Uptime
    , incidentToUptime
    )
where

import RIO

import PDUp.Incident

data Uptime

instance Semigroup Uptime where
    _a <> _b = undefined

instance Monoid Uptime where
    mempty = undefined

instance Display Uptime where
    textDisplay = undefined

incidentToUptime :: Incident -> Uptime
incidentToUptime = undefined
