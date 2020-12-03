module AdventOfCode.Day3Spec
  ( spec,
  )
where

import AdventOfCode.Day3
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
  -- it "process1" $ do
  --   process1 0 `shouldBe` 0
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
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
