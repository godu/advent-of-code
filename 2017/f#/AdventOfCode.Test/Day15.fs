namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day15 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day15 (input: string, expect: string) =
        Assert.AreEqual(Day15.answer input, expect);
