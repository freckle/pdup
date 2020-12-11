module PDUp.OutagesSpec
  ( spec
  )
where

import RIO

import PDUp.Outages
import RIO.Time
import Test.Hspec

spec :: Spec
spec = do
  describe "Outages" $ do
    it "doesn't count more than a minute outage for any given minute" $ do
      let
        outages =
          emptyOutages
            `addOutage` outageAtMinute 0 1
            `addOutage` outageAtMinute 2 4
            `addOutage` outageAtMinute 3 6
            `addOutage` outageAtMinute 8 9

      outagesMinutes outages `shouldBe` 6

outageAtMinute :: Integer -> Integer -> Outage
outageAtMinute began resolved = Outage
  { outageBegan = timeAtMinute began
  , outageResolved = timeAtMinute resolved
  , outageSummaries = [""]
  }
 where
  timeAtMinute minute = UTCTime
    { utctDay = fromGregorian 2020 4 16
    , utctDayTime = secondsToDiffTime $ minute * 60
    }
