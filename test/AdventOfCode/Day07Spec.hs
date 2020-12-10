module AdventOfCode.Day07Spec
  ( spec,
  )
where

import AdventOfCode.Day07
  ( Bag,
    Rules,
    isContainedIn,
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
  it "isContainedIn" $ do
    isContainedIn rules "shiny gold" "light red" `shouldBe` True
    isContainedIn rules "shiny gold" "dark orange" `shouldBe` True
    isContainedIn rules "shiny gold" "bright white" `shouldBe` True
    isContainedIn rules "shiny gold" "muted yellow" `shouldBe` True
    isContainedIn rules "shiny gold" "shiny gold" `shouldBe` False
    isContainedIn rules "shiny gold" "dark olive" `shouldBe` False
    isContainedIn rules "shiny gold" "vibrant plum" `shouldBe` False
    isContainedIn rules "shiny gold" "faded blue" `shouldBe` False
    isContainedIn rules "shiny gold" "dotted black" `shouldBe` False
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

  it "run2" $ do
    run2
      "shiny gold bags contain 2 dark red bags.\n\
      \dark red bags contain 2 dark orange bags.\n\
      \dark orange bags contain 2 dark yellow bags.\n\
      \dark yellow bags contain 2 dark green bags.\n\
      \dark green bags contain 2 dark blue bags.\n\
      \dark blue bags contain 2 dark violet bags.\n\
      \dark violet bags contain no other bags.\n"
      `shouldBe` "126"
