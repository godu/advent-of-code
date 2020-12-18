module AdventOfCode.Day17Spec
  ( spec,
  )
where

import AdventOfCode.Day17
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
      ".#.\n\
      \..#\n\
      \###\n"
      `shouldBe` "112"
  it "run2" $ do
    run2
      ".#.\n\
      \..#\n\
      \###\n"
      `shouldBe` "848"
