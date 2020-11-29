module AdventOfCode.Day1Spec
  ( spec,
  )
where

import AdventOfCode.Day1
  ( run,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  it "Day1" $ do
    run "foo" `shouldBe` "foo"
