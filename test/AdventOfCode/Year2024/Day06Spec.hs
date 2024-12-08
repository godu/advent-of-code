module AdventOfCode.Year2024.Day06Spec
  ( spec,
  )
where

import AdventOfCode.Year2024.Day06
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
          [ "....#.....",
            ".........#",
            "..........",
            "..#.......",
            ".......#..",
            "..........",
            ".#..^.....",
            "........#.",
            "#.........",
            "......#..."
          ]
      )
      `shouldBe` "41"

  it "run2" $ do
    run2
      ( unlines
          [ "....#.....",
            ".........#",
            "..........",
            "..#.......",
            ".......#..",
            "..........",
            ".#..^.....",
            "........#.",
            "#.........",
            "......#..."
          ]
      )
      `shouldBe` "6"
