namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day13 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day13 (input: string, expect: string) =
        Assert.AreEqual(Day13.answer input, expect);
