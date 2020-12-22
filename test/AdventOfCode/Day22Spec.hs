module AdventOfCode.Day22Spec
  ( spec,
  )
where

import AdventOfCode.Day22
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
      "Player 1:\n\
      \9\n\
      \2\n\
      \6\n\
      \3\n\
      \1\n\
      \\n\
      \Player 2:\n\
      \5\n\
      \8\n\
      \4\n\
      \7\n\
      \10\n"
      `shouldBe` "306"
  it "run2" $ do
    run2 "" `shouldBe` ""
