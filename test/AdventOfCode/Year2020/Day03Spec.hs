module AdventOfCode.Year2020.Day03Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day03
  ( process1,
    process2,
    run1,
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
      "..##.......\n\
      \#...#...#..\n\
      \.#....#..#.\n\
      \..#.#...#.#\n\
      \.#...##..#.\n\
      \..#.##.....\n\
      \.#.#.#....#\n\
      \.#........#\n\
      \#.##...#...\n\
      \#...##....#\n\
      \.#..#...#.#"
      `shouldBe` "7"
  it "run2" $ do
    run2
      "..##.......\n\
      \#...#...#..\n\
      \.#....#..#.\n\
      \..#.#...#.#\n\
      \.#...##..#.\n\
      \..#.##.....\n\
      \.#.#.#....#\n\
      \.#........#\n\
      \#.##...#...\n\
      \#...##....#\n\
      \.#..#...#.#"
      `shouldBe` "336"
