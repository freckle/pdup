module Main
    ( main
    )
where

import RIO

import PDUp.Incident
import PDUp.Options
import PDUp.Token
import PDUp.Uptime
import RIO.Text (pack)
import System.Environment (getEnv)

main :: IO ()
main = do
    token <- Token . pack <$> getEnv "PAGERDUTY_TOKEN"
    Options {..} <- parseOptions

    runSimpleApp $ do
        incidents <- fetchIncidents token oStart oEnd
        let uptime = foldMap incidentToUptime incidents
        logInfo $ display uptime
