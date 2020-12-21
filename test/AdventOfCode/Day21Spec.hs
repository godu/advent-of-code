module AdventOfCode.Day21Spec
  ( spec,
  )
where

import AdventOfCode.Day21
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
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
      \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
      \sqjhc fvjkl (contains soy)\n\
      \sqjhc mxmxvkd sbzzf (contains fish)\n"
      `shouldBe` "5"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
