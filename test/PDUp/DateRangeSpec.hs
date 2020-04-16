module PDUp.DateRangeSpec
  ( spec
  )
where

import RIO

import PDUp.DateRange
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "dateRange" $ do
    it "asserts since <= until" $ property $ \(since, until) ->
      if since <= until
        then
          let
            range = dateRange since until
            since' = dateRangeSince <$> range
            until' = dateRangeUntil <$> range
          in since' == Right since && until' == Right until
        else isLeft $ dateRange since until
