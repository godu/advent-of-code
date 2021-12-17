module AdventOfCode.Year2021.Day16Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day16
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
    run1 (unlines ["D2FE28"]) `shouldBe` "6"
    run1 (unlines ["38006F45291200"]) `shouldBe` "9"
    run1 (unlines ["EE00D40C823060"]) `shouldBe` "14"

    run1 (unlines ["8A004A801A8002F478"]) `shouldBe` "16"
    run1 (unlines ["620080001611562C8802118E34"]) `shouldBe` "12"
    run1 (unlines ["C0015000016115A2E0802F182340"]) `shouldBe` "23"
    run1 (unlines ["A0016C880162017C3686B18A3D4780"]) `shouldBe` "31"

  it "run2" $ do
    run2 (unlines ["C200B40A82"]) `shouldBe` "3"
    run2 (unlines ["04005AC33890"]) `shouldBe` "54"
    run2 (unlines ["880086C3E88112"]) `shouldBe` "7"
    run2 (unlines ["CE00C43D881120"]) `shouldBe` "9"
    run2 (unlines ["D8005AC2A8F0"]) `shouldBe` "1"
    run2 (unlines ["F600BC2D8F"]) `shouldBe` "0"
    run2 (unlines ["9C005AC2F8F0"]) `shouldBe` "0"
    run2 (unlines ["9C0141080250320F1802104A08"]) `shouldBe` "1"
