module AdventOfCode.Year2023.Day06Spec
  ( spec,
  )
where

import AdventOfCode.Year2023.Day06
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
      ( unlines
          [ "Time:      7  15   30",
            "Distance:  9  40  200"
          ]
      )
      `shouldBe` "288"

  it "run2" $ do
    run2
      ( unlines
          [ "Time:      7  15   30",
            "Distance:  9  40  200"
          ]
      )
      `shouldBe` "71503"
    run2
      ( unlines
          [ "Time:        47     84     74     67",
            "Distance:   207   1394   1209   1014"
          ]
      )
      `shouldBe` "38220708"
