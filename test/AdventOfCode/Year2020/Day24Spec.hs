module AdventOfCode.Year2020.Day24Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day24
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
      "sesenwnenenewseeswwswswwnenewsewsw\n\
      \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
      \seswneswswsenwwnwse\n\
      \nwnwneseeswswnenewneswwnewseswneseene\n\
      \swweswneswnenwsewnwneneseenw\n\
      \eesenwseswswnenwswnwnwsewwnwsene\n\
      \sewnenenenesenwsewnenwwwse\n\
      \wenwwweseeeweswwwnwwe\n\
      \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
      \neeswseenwwswnwswswnw\n\
      \nenwswwsewswnenenewsenwsenwnesesenew\n\
      \enewnwewneswsewnwswenweswnenwsenwsw\n\
      \sweneswneswneneenwnewenewwneswswnese\n\
      \swwesenesewenwneswnwwneseswwne\n\
      \enesenwswwswneneswsenwnewswseenwsese\n\
      \wnwnesenesenenwwnenwsewesewsesesew\n\
      \nenewswnwewswnenesenwnesewesw\n\
      \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
      \neswnwewnwnwseenwseesewsenwsweewe\n\
      \wseweeenwnesenwwwswnew\n"
      `shouldBe` "10"
  it "run2" $ do
    run2
      "sesenwnenenewseeswwswswwnenewsewsw\n\
      \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
      \seswneswswsenwwnwse\n\
      \nwnwneseeswswnenewneswwnewseswneseene\n\
      \swweswneswnenwsewnwneneseenw\n\
      \eesenwseswswnenwswnwnwsewwnwsene\n\
      \sewnenenenesenwsewnenwwwse\n\
      \wenwwweseeeweswwwnwwe\n\
      \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
      \neeswseenwwswnwswswnw\n\
      \nenwswwsewswnenenewsenwsenwnesesenew\n\
      \enewnwewneswsewnwswenweswnenwsenwsw\n\
      \sweneswneswneneenwnewenewwneswswnese\n\
      \swwesenesewenwneswnwwneseswwne\n\
      \enesenwswwswneneswsenwnewswseenwsese\n\
      \wnwnesenesenenwwnenwsewesewsesesew\n\
      \nenewswnwewswnenesenwnesewesw\n\
      \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
      \neswnwewnwnwseenwseesewsenwsweewe\n\
      \wseweeenwnesenwwwswnew\n"
      `shouldBe` "2208"
