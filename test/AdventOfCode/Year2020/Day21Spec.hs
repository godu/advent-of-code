module AdventOfCode.Year2020.Day21Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day21
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
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
      \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
      \sqjhc fvjkl (contains soy)\n\
      \sqjhc mxmxvkd sbzzf (contains fish)\n"
      `shouldBe` "5"
  it "run2" $ do
    run2
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
      \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
      \sqjhc fvjkl (contains soy)\n\
      \sqjhc mxmxvkd sbzzf (contains fish)\n"
      `shouldBe` "mxmxvkd,sqjhc,fvjkl"
