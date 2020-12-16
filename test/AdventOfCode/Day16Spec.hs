module AdventOfCode.Day16Spec
  ( spec,
  )
where

import AdventOfCode.Day16
  ( Note (Note),
    Rule (Rule),
    Ticket (Ticket),
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
  it "Rule" $ do
    read "class: 1-3 or 5-7" `shouldBe` Rule "class" [(1, 3), (5, 7)]
    read "row: 6-11 or 33-44" `shouldBe` Rule "row" [(6, 11), (33, 44)]
    read "seat: 13-40 or 45-50" `shouldBe` Rule "seat" [(13, 40), (45, 50)]
  it "Note" $ do
    read
      "class: 1-3 or 5-7\n\
      \row: 6-11 or 33-44\n\
      \seat: 13-40 or 45-50\n\
      \\n\
      \your ticket:\n\
      \7,1,14\n\
      \\n\
      \nearby tickets:\n\
      \7,3,47\n\
      \40,4,50\n\
      \55,2,20\n\
      \38,6,12\n"
      `shouldBe` Note
        [ Rule "class" [(1, 3), (5, 7)],
          Rule "row" [(6, 11), (33, 44)],
          Rule "seat" [(13, 40), (45, 50)]
        ]
        (Ticket [7, 1, 14])
        [ Ticket [7, 3, 47],
          Ticket [40, 4, 50],
          Ticket [55, 2, 20],
          Ticket [38, 6, 12]
        ]
  it "run1" $ do
    run1
      "class: 1-3 or 5-7\n\
      \row: 6-11 or 33-44\n\
      \seat: 13-40 or 45-50\n\
      \\n\
      \your ticket:\n\
      \7,1,14\n\
      \\n\
      \nearby tickets:\n\
      \7,3,47\n\
      \40,4,50\n\
      \55,2,20\n\
      \38,6,12\n"
      `shouldBe` "71"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
