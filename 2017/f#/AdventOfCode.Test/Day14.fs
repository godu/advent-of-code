namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day14 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day14 (input: string, expect: string) =
        Assert.AreEqual(Day14.answer input, expect);
