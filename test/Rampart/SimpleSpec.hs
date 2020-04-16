module Rampart.SimpleSpec
  ( spec
  )
where

import Prelude

import Rampart.Simple
import Test.Hspec

spec :: Spec
spec = do
  describe "relate" $ do
    it "simplifies the real Relation type" $ do
      -- https://hackage.haskell.org/package/rampart-1.0.0.3/docs/Rampart.html#v:relate
      relate (toInterval @Int (1, 2)) (toInterval (3, 7)) `shouldBe` Before
      relate (toInterval @Int (2, 3)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (2, 4)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (2, 7)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (2, 8)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (3, 4)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (3, 7)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (3, 8)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (4, 6)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (6, 7)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (6, 8)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (7, 8)) (toInterval (3, 7)) `shouldBe` Concurrent
      relate (toInterval @Int (8, 9)) (toInterval (3, 7)) `shouldBe` After
