module AdventOfCode.Year2020.Day23Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day23 (process1, process2, run1, run2)
import qualified Data.Vector as V (fromList)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  -- it "go" $ do
  --   go 0 0 (V.fromList [3, 8, 9, 1, 2, 5, 4, 6, 7])
  --     `shouldBe` V.fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]
  --   go 1 0 (V.fromList [3, 8, 9, 1, 2, 5, 4, 6, 7])
  --     `shouldBe` V.fromList [3, 2, 8, 9, 1, 5, 4, 6, 7]
  --   go 1 1 (V.fromList [3, 2, 8, 9, 1, 5, 4, 6, 7])
  --     `shouldBe` V.fromList [3, 2, 5, 4, 6, 7, 8, 9, 1]
  --   go 1 2 (V.fromList [3, 2, 5, 4, 6, 7, 8, 9, 1])
  --     `shouldBe` V.fromList [7, 2, 5, 8, 9, 1, 3, 4, 6]
  --   go 1 3 (V.fromList [7, 2, 5, 8, 9, 1, 3, 4, 6])
  --     `shouldBe` V.fromList [3, 2, 5, 8, 4, 6, 7, 9, 1]
  --   go 1 4 (V.fromList [3, 2, 5, 8, 4, 6, 7, 9, 1])
  --     `shouldBe` V.fromList [9, 2, 5, 8, 4, 1, 3, 6, 7]
  --   go 1 5 (V.fromList [9, 2, 5, 8, 4, 1, 3, 6, 7])
  --     `shouldBe` V.fromList [7, 2, 5, 8, 4, 1, 9, 3, 6]
  --   go 1 6 (V.fromList [7, 2, 5, 8, 4, 1, 9, 3, 6])
  --     `shouldBe` V.fromList [8, 3, 6, 7, 4, 1, 9, 2, 5]
  --   go 1 7 (V.fromList [8, 3, 6, 7, 4, 1, 9, 2, 5])
  --     `shouldBe` V.fromList [7, 4, 1, 5, 8, 3, 9, 2, 6]

  it "process1" $ do
    process1 0 [3, 8, 9, 1, 2, 5, 4, 6, 7]
      `shouldBe` [2, 5, 4, 6, 7, 3, 8, 9]
    process1 10 [3, 8, 9, 1, 2, 5, 4, 6, 7]
      `shouldBe` [9, 2, 6, 5, 8, 3, 7, 4]
    process1 100 [3, 8, 9, 1, 2, 5, 4, 6, 7]
      `shouldBe` [6, 7, 3, 8, 4, 5, 2, 9]

  it "run1" $ do
    run1 "389125467" `shouldBe` "67384529"

  it "process2" $ do
    process2 100 [3, 8, 9, 1, 2, 5, 4, 6, 7] `shouldBe` 12
    process2 10000000 [3, 8, 9, 1, 2, 5, 4, 6, 7] `shouldBe` 149245887792
