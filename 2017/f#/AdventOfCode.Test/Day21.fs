namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day21 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day21 (input: string, expect: string) =
        Assert.AreEqual(Day21.answer input, expect);
