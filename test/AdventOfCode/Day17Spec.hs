module AdventOfCode.Day17Spec
  ( spec,
  )
where

import AdventOfCode.Day17
  ( Coordinate (Coordinate),
    neighbors,
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
  it "neighbors" $ do
    neighbors (Coordinate 1 2 3)
      `shouldBe` [ Coordinate 0 1 2,
                   Coordinate 0 1 3,
                   Coordinate 0 1 4,
                   Coordinate 0 2 2,
                   Coordinate 0 2 3,
                   Coordinate 0 2 4,
                   Coordinate 0 3 2,
                   Coordinate 0 3 3,
                   Coordinate 0 3 4,
                   Coordinate 1 1 2,
                   Coordinate 1 1 3,
                   Coordinate 1 1 4,
                   Coordinate 1 2 2,
                   Coordinate 1 2 4,
                   Coordinate 1 3 2,
                   Coordinate 1 3 3,
                   Coordinate 1 3 4,
                   Coordinate 2 1 2,
                   Coordinate 2 1 3,
                   Coordinate 2 1 4,
                   Coordinate 2 2 2,
                   Coordinate 2 2 3,
                   Coordinate 2 2 4,
                   Coordinate 2 3 2,
                   Coordinate 2 3 3,
                   Coordinate 2 3 4
                 ]
  it "run1" $ do
    run1
      ".#.\n\
      \..#\n\
      \###\n"
      `shouldBe` "112"
  it "run2" $ do
    run2 "" `shouldBe` ""
