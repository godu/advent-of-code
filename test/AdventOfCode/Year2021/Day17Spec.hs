module AdventOfCode.Year2021.Day17Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day17
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
    run1 (unlines ["target area: x=20..30, y=-10..-5"]) `shouldBe` "45"

-- it "run2" $ do
--   run2 (unlines []) `shouldBe` ""
