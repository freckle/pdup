module PDUp.OutagesTableSpec
  ( spec
  )
where

import RIO

import PDUp.Outages
import PDUp.OutagesTable
import qualified Prelude as Unsafe
import RIO.Text (pack, unpack)
import qualified RIO.Text.Partial as T (splitOn)
import RIO.Time
import Test.Hspec

spec :: Spec
spec = do
  describe "tabulateOutages" $ do
    it "minimizes repeated information" $ do
      let
        outages = makeOutages
          [ ("2020-02-18 19:46:07 UTC", "2020-02-18 19:48:10 UTC")
          , ("2020-02-17 15:32:55 UTC", "2020-02-17 15:38:55 UTC")
          , ("2020-02-17 15:19:53 UTC", "2020-02-17 15:30:53 UTC")
          , ("2020-02-17 14:31:53 UTC", "2020-02-17 14:36:54 UTC")
          , ("2020-02-17 14:25:54 UTC", "2020-02-17 14:30:55 UTC")
          , ("2020-02-14 23:50:53 UTC", "2020-02-14 23:55:50 UTC")
          , ("2020-02-14 18:47:53 UTC", "2020-02-14 18:52:53 UTC")
          , ("2020-02-14 12:50:52 UTC", "2020-02-14 12:54:52 UTC")
          , ("2020-02-13 16:37:42 UTC", "2020-02-13 16:48:43 UTC")
          , ("2020-02-07 23:34:08 UTC", "2020-02-07 23:38:09 UTC")
          , ("2020-02-07 00:00:56 UTC", "2020-02-07 00:09:55 UTC")
          ]

        expected = makeOutagesTable
          [ ("N2020-02-07", "00:00:56", "S2020-02-07", "00:09:55", 9)
          , ("S2020-02-07", "23:34:08", "S2020-02-07", "23:38:09", 4)
          , ("N2020-02-13", "16:37:42", "S2020-02-13", "16:48:43", 11)
          , ("N2020-02-14", "12:50:52", "S2020-02-14", "12:54:52", 4)
          , ("S2020-02-14", "18:47:53", "S2020-02-14", "18:52:53", 5)
          , ("S2020-02-14", "23:50:53", "S2020-02-14", "23:55:50", 5)
          , ("N2020-02-17", "14:25:54", "S2020-02-17", "14:30:55", 5)
          , ("S2020-02-17", "14:31:53", "S2020-02-17", "14:36:54", 5)
          , ("S2020-02-17", "15:19:53", "S2020-02-17", "15:30:53", 11)
          , ("S2020-02-17", "15:32:55", "S2020-02-17", "15:38:55", 6)
          , ("N2020-02-18", "19:46:07", "S2020-02-18", "19:48:10", 2)
          ]

      tabulateOutages outages `shouldBe` expected

makeOutages :: [(String, String)] -> Outages
makeOutages = Outages . map outage
 where
  outage (began, resolved) = Outage
    { outageBegan = Unsafe.read began
    , outageResolved = Unsafe.read resolved
    , outageSummaries = []
    }

makeOutagesTable :: [(String, String, String, String, Integer)] -> OutagesTable
makeOutagesTable = OutagesTable . map outageRow
 where
  outageRow (beganDay, beganTime, resolvedDay, resolvedTime, duration) =
    OutageRow
      { outageBeganDay = outageDay beganDay
      , outageBeganTime = readDiffTime beganTime
      , outageResolvedDay = outageDay resolvedDay
      , outageResolvedTime = readDiffTime resolvedTime
      , outageDuration = duration
      , outageSummaries = []
      }

  outageDay = \case
    ('N' : x) -> NewDay $ Unsafe.read x
    ('S' : x) -> SameAs $ Unsafe.read x
    _ -> error "Invalid OutageDay string"

readDiffTime :: String -> DiffTime
readDiffTime = fromParts . map (Unsafe.read . unpack) . T.splitOn ":" . pack
 where
  fromParts :: [Integer] -> DiffTime
  fromParts [h, m, s] = secondsToDiffTime $ (h * 60 * 60) + (m * 60) + s
  fromParts _ = error "Incorrect :-delimited parts"
