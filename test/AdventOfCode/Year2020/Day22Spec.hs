module AdventOfCode.Year2020.Day22Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day22
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
    run2
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
      `shouldBe` "291"
    run2
      "Player 1:\n\
      \43\n\
      \19\n\
      \\n\
      \Player 2:\n\
      \2\n\
      \29\n\
      \14\n"
      `shouldBe` "105"
