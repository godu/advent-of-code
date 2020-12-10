module AdventOfCode.Day02Spec
  ( spec,
  )
where

import AdventOfCode.Day02
  ( Input (Input),
    process1,
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
  it "run1" $ do
    run1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` "2"
  it "process1" $ do
    process1
      [ Input 1 3 'a' "abcde",
        Input 1 3 'b' "cdefg",
        Input 2 9 'c' "ccccccccc"
      ]
      `shouldBe` 2

  it "run2" $ do
    run2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` "1"
  it "process2" $ do
    process2
      [ Input 1 3 'a' "abcde",
        Input 1 3 'b' "cdefg",
        Input 2 9 'c' "ccccccccc"
      ]
      `shouldBe` 1
