module Main
  ( main
  )
where

import Conduit
import Control.Error.Util (note)
import LoadEnv (loadEnv)
import Options.Applicative.Simple
import PDUp.DateRange
import PDUp.Incident
import PDUp.Outages
import PDUp.OutagesTable
import PDUp.Token
import RIO
import RIO.Text (pack)
import RIO.Time
import System.Environment (getEnv)

data Options = Days Natural | Range UTCTime UTCTime

-- brittany-disable-next-binding

parser :: UTCTime -> Parser Options
parser now = asum
    [ Days
        <$> option auto
            (  short 'd'
            <> long "days"
            <> metavar "N"
            <> help "Query over previus N days from now"
            )
    , Range
        <$> option (eitherReader readDaySince)
            (  short 's'
            <> long "since"
            <> metavar "YYYY-MM-DD"
            <> help "Query since start of date"
            )
        <*> option (eitherReader readDayUntil)
            (  short 'u'
            <> long "until"
            <> metavar "YYYY-MM-DD"
            <> help "Query until end of date"
            <> value now
            <> showDefault
            )
    ]

readDaySince :: String -> Either String UTCTime
readDaySince = readDayTime . (<> " 00:00")

readDayUntil :: String -> Either String UTCTime
readDayUntil = readDayTime . (<> " 23:59")

readDayTime :: String -> Either String UTCTime
readDayTime x = note err $ parseTimeM True defaultTimeLocale fmt x
 where
  err = x <> " did not parse as time format " <> fmt
  fmt = "%Y-%m-%d %H:%M"

main :: IO ()
main = do
  loadEnv
  token <- Token . pack <$> getEnv "PAGERDUTY_TOKEN"

  now <- liftIO getCurrentTime
  options <- parseOptions $ parser now

  runSimpleApp $ case options of
    Days days -> do
      let
        diff = negate $ 60 * 60 * 24 * fromIntegral days
        since = addUTCTime diff now
      logInfo $ "Last " <> displayShow days <> " days' outages"
      range <- either throwString pure $ dateRange since now
      run token range

    Range since until -> do
      logInfo
        $ "Outages between "
        <> displayShow since
        <> " and "
        <> displayShow until
      range <- either throwString pure $ dateRange since now
      run token range


run :: HasLogFunc env => Token -> DateRange -> RIO env ()
run token range = do
  outages <-
    runConduit
    $ sourceIncidents token range
    .| concatC
    .| iterMC (logDebug . displayShow)
    .| filterC ((== High) . incidentUrgency)
    .| foldlC (addOutageFromIncident until) emptyOutages

  logInfo $ display $ tabulateOutages outages

  let
    outage :: Integer
    outage = outagesMinutes outages

  logInfo
    $ "  "
    <> display outage
    <> " minutes in outage (out of "
    <> display total
    <> "): "
    <> display (nines outage total)
    <> "\n"
 where
  since = dateRangeSince range
  until = dateRangeUntil range
  total = round $ diffUTCTime until since / 60

nines :: Integer -> Integer -> Double
nines a b = (fromIntegral (b - a) / fromIntegral b) * 100
