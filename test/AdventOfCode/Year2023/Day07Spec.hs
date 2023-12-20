module AdventOfCode.Year2023.Day07Spec
  ( spec,
  )
where

import AdventOfCode.Year2023.Day07
  ( run1,
    run2,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  it "run1" $ do
    run1
      ( unlines
          [ "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483"
          ]
      )
      `shouldBe` "6440"

  it "run2" $ do
    run2
      ( unlines
          [ "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483"
          ]
      )
      `shouldBe` "5905"
