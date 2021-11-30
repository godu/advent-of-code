module AdventOfCode.Year2020.Day06Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day06
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
      "abc\n\
      \\n\
      \a\n\
      \b\n\
      \c\n\
      \\n\
      \ab\n\
      \ac\n\
      \\n\
      \a\n\
      \a\n\
      \a\n\
      \a\n\
      \\n\
      \b"
      `shouldBe` "11"
  it "run2" $ do
    run2
      "abc\n\
      \\n\
      \a\n\
      \b\n\
      \c\n\
      \\n\
      \ab\n\
      \ac\n\
      \\n\
      \a\n\
      \a\n\
      \a\n\
      \a\n\
      \\n\
      \b"
      `shouldBe` "6"
