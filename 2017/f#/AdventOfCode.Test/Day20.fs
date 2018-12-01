namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day20 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day20 (input: string, expect: string) =
        Assert.AreEqual(Day20.answer input, expect);
