module AdventOfCode.Day11Spec
  ( spec,
  )
where

import AdventOfCode.Day11
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
      "L.LL.LL.LL\n\
      \LLLLLLL.LL\n\
      \L.L.L..L..\n\
      \LLLL.LL.LL\n\
      \L.LL.LL.LL\n\
      \L.LLLLL.LL\n\
      \..L.L.....\n\
      \LLLLLLLLLL\n\
      \L.LLLLLL.L\n\
      \L.LLLLL.LL\n"
      `shouldBe` "37"
  it "run2" $ do
    run2
      "L.LL.LL.LL\n\
      \LLLLLLL.LL\n\
      \L.L.L..L..\n\
      \LLLL.LL.LL\n\
      \L.LL.LL.LL\n\
      \L.LLLLL.LL\n\
      \..L.L.....\n\
      \LLLLLLLLLL\n\
      \L.LLLLLL.L\n\
      \L.LLLLL.LL\n"
      `shouldBe` "26"
