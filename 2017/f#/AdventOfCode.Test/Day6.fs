namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day6 () =
    [<DataTestMethod>]
    [<DataRow("0\t2\t7\t0", 5)>]
    [<DataRow("2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14", 3156)>]
    member this.Day6 (input: string, expect: int) =
        Assert.AreEqual(Day6.answer input, expect);
