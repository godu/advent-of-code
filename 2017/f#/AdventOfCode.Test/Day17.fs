namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day17 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day17 (input: string, expect: string) =
        Assert.AreEqual(Day17.answer input, expect);
