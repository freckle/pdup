module PDUp.Options
    ( Options(..)
    , parseOptions
    )
where

import RIO

import RIO.Time (UTCTime)

data Options = Options
    { oStart :: UTCTime
    , oEnd :: UTCTime
    }

parseOptions :: IO Options
parseOptions = undefined
