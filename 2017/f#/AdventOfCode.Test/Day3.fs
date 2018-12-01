namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day3 () =
    [<DataTestMethod>]
    [<DataRow(1, 0)>]
    [<DataRow(12, 3)>]
    [<DataRow(23, 2)>]
    [<DataRow(1024, 31)>]
    [<DataRow(289326, 419)>]
    member this.Day3 (input: int, expect: int) =
        Assert.AreEqual(Day3.answer input, expect);
