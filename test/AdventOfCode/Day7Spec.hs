module AdventOfCode.Day7Spec
  ( spec,
  )
where

import AdventOfCode.Day7
  ( Bag,
    Rules,
    process1,
    process2,
    ruleParser,
    rulesParser,
    run1,
    run2,
  )
import Data.Map (fromList)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Text.Read (minPrec, readPrec_to_S)

rules :: Rules
rules =
  fromList
    [ ("light red", ["bright white", "muted yellow", "muted yellow"]),
      ("dark orange", ["bright white", "bright white", "bright white", "muted yellow", "muted yellow", "muted yellow", "muted yellow"]),
      ("bright white", ["shiny gold"]),
      ("muted yellow", ["shiny gold", "shiny gold", "faded blue", "faded blue", "faded blue", "faded blue", "faded blue", "faded blue", "faded blue", "faded blue", "faded blue"]),
      ("shiny gold", ["dark olive", "vibrant plum", "vibrant plum"]),
      ("dark olive", ["faded blue", "faded blue", "faded blue", "dotted black", "dotted black", "dotted black", "dotted black"]),
      ("vibrant plum", ["faded blue", "faded blue", "faded blue", "faded blue", "faded blue", "dotted black", "dotted black", "dotted black", "dotted black", "dotted black", "dotted black"]),
      ("faded blue", []),
      ("dotted black", [])
    ]

spec :: Spec
spec = do
  it "ruleParser" $
    do
      readPrec_to_S ruleParser minPrec "light red bags contain 1 bright white bag, 2 muted yellow bags."
        `shouldBe` [ ( ( "light red",
                         [ "bright white",
                           "muted yellow",
                           "muted yellow"
                         ]
                       ),
                       ""
                     )
                   ]
      readPrec_to_S ruleParser minPrec "dotted black bags contain no other bags."
        `shouldBe` [ ( ( "dotted black",
                         []
                       ),
                       ""
                     )
                   ]

  it "rulesParser" $ do
    filter
      ((== "") . snd)
      ( readPrec_to_S
          rulesParser
          minPrec
          "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
          \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
          \bright white bags contain 1 shiny gold bag.\n\
          \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
          \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
          \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
          \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
          \faded blue bags contain no other bags.\n\
          \dotted black bags contain no other bags.\n"
      )
      `shouldBe` [(rules, "")]
  it "process1" $ do
    process1 rules "shiny gold" "light red" `shouldBe` True
    process1 rules "shiny gold" "dark orange" `shouldBe` True
    process1 rules "shiny gold" "bright white" `shouldBe` True
    process1 rules "shiny gold" "muted yellow" `shouldBe` True
    process1 rules "shiny gold" "shiny gold" `shouldBe` False
    process1 rules "shiny gold" "dark olive" `shouldBe` False
    process1 rules "shiny gold" "vibrant plum" `shouldBe` False
    process1 rules "shiny gold" "faded blue" `shouldBe` False
    process1 rules "shiny gold" "dotted black" `shouldBe` False
  it "run1" $ do
    run1
      "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
      \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
      \bright white bags contain 1 shiny gold bag.\n\
      \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
      \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
      \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
      \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
      \faded blue bags contain no other bags.\n\
      \dotted black bags contain no other bags.\n"
      `shouldBe` "4"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
