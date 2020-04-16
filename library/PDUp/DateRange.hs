module PDUp.DateRange
    ( DateRange
    , dateRange
    , dateRangeSince
    , dateRangeUntil
    )
where

import RIO

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
