module AdventOfCode.Day6Spec
  ( spec,
  )
where

import AdventOfCode.Day6
  ( process2,
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
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
