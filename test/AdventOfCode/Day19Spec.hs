module AdventOfCode.Day19Spec
  ( spec,
  )
where

import AdventOfCode.Day19
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
      "0: 4 1 5\n\
      \1: 2 3 | 3 2\n\
      \2: 4 4 | 5 5\n\
      \3: 4 5 | 5 4\n\
      \4: \"a\"\n\
      \5: \"b\"\n\
      \\n\
      \ababbb\n\
      \bababa\n\
      \abbbab\n\
      \aaabbb\n\
      \aaaabbb\n"
      `shouldBe` "2"
  it "run2" $ do
    run2 "" `shouldBe` ""
