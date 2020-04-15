module PDUp.Options
    ( Options(..)
    , parseOptions
    )
where

import RIO

import RIO.Time (UTCTime, addUTCTime, getCurrentTime)

data Options = Options
    { oSince :: UTCTime
    , oUntil :: UTCTime
    }

parseOptions :: IO Options
parseOptions = do
    now <- getCurrentTime

    let since = addUTCTime (negate $ 60 * 60 * 24 * 90) now

    pure Options { oSince = since, oUntil = now }
