module PDUp.OutagesTable
  ( OutagesTable(..)
  , OutageRow(..)
  , OutageDay(..)
  , tabulateOutages
  )
where

import RIO

import PDUp.Outages
import qualified RIO.Text as T
import RIO.Time

newtype OutagesTable = OutagesTable [OutageRow]
  deriving stock (Eq, Show)

instance Display OutagesTable where
  textDisplay = \case
    OutagesTable [] -> divider <> "\n" <> divider
    OutagesTable rows ->
      divider <> "\n" <> divideRow (map textDisplay rows) <> "\n" <> divider
   where
    divideRow = T.intercalate $ "\n" <> divider <> "\n"
    divider = "  +------------+----------+------------+----------+"

data OutageRow = OutageRow
  { outageBeganDay :: OutageDay
  , outageBeganTime :: DiffTime
  , outageResolvedDay :: OutageDay
  , outageResolvedTime :: DiffTime
  , outageDuration :: Integer
  }
  deriving stock (Eq, Show)

instance Display OutageRow where
  display OutageRow {..} = mconcat
    [ "  "
    , "| " <> display outageBeganDay <> " "
    , "| " <> displayDiffTime outageBeganTime <> " "
    , "| " <> display outageResolvedDay <> " "
    , "| " <> displayDiffTime outageResolvedTime <> " "
    , "| " <> display outageDuration <> " minute(s)"
    ]

data OutageDay = NewDay Day | SameAs Day
  deriving stock (Eq, Show)

instance Display OutageDay where
  display = \case
    NewDay x -> displayShow x
    SameAs _ -> "          "

tabulateOutages :: Outages -> OutagesTable
tabulateOutages =
  OutagesTable . reverse . foldl' outageRow [] . reverse . unOutages

outageRow :: [OutageRow] -> Outage -> [OutageRow]
outageRow rows o@Outage {..} = case rows of
  [] ->
    [ OutageRow
        { outageBeganDay = NewDay beganDay
        , outageBeganTime = beganTime
        , outageResolvedDay = if resolvedDay == beganDay
          then SameAs beganDay
          else NewDay resolvedDay
        , outageResolvedTime = resolvedTime
        , outageDuration = outageMinutes o
        }
    ]

  (row : rest) ->
    let
      rowResolvedDay = case outageResolvedDay row of
        NewDay x -> x
        SameAs x -> x
    in
      OutageRow
        { outageBeganDay = if resolvedDay == rowResolvedDay
          then SameAs rowResolvedDay
          else NewDay resolvedDay
        , outageBeganTime = beganTime
        , outageResolvedDay = if resolvedDay == beganDay
          then SameAs beganDay
          else NewDay resolvedDay
        , outageResolvedTime = resolvedTime
        , outageDuration = outageMinutes o
        }
      : row
      : rest
 where
  beganDay = utctDay outageBegan
  beganTime = utctDayTime outageBegan
  resolvedDay = utctDay outageResolved
  resolvedTime = utctDayTime outageResolved

displayDiffTime :: DiffTime -> Utf8Builder
displayDiffTime d =
  let
    s0 = diffTimeToSeconds d
    (m0, s1) = s0 `divMod` 60
    (h, m1) = m0 `divMod` 60
  in zeroPad h <> ":" <> zeroPad m1 <> ":" <> zeroPad s1
 where
  zeroPad n
    | n < 10 = "0" <> displayShow n
    | otherwise = displayShow n

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds =
  round @Double . (/ 1e+12) . fromIntegral . diffTimeToPicoseconds
