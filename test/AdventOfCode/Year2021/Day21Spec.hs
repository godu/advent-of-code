module AdventOfCode.Year2021.Day21Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day21
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
          [ "Player 1 starting position: 4",
            "Player 2 starting position: 8"
          ]
      )
      `shouldBe` "739785"

  it "run2" $ do
    run2
      ( unlines
          [ "Player 1 starting position: 4",
            "Player 2 starting position: 8"
          ]
      )
      `shouldBe` "444356092776315"
