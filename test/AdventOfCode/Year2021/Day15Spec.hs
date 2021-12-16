module AdventOfCode.Year2021.Day15Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day15
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
          [ "1163751742",
            "1381373672",
            "2136511328",
            "3694931569",
            "7463417111",
            "1319128137",
            "1359912421",
            "3125421639",
            "1293138521",
            "2311944581"
          ]
      )
      `shouldBe` "40"
    run1
      ( unlines
          [ "19111",
            "11191"
          ]
      )
      `shouldBe` "7"

  it "run2" $ do
    run2
      ( unlines
          [ "1163751742",
            "1381373672",
            "2136511328",
            "3694931569",
            "7463417111",
            "1319128137",
            "1359912421",
            "3125421639",
            "1293138521",
            "2311944581"
          ]
      )
      `shouldBe` "315"
