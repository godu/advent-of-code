namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day18 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day18 (input: string, expect: string) =
        Assert.AreEqual(Day18.answer input, expect);
