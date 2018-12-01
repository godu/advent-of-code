namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day22 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day22 (input: string, expect: string) =
        Assert.AreEqual(Day22.answer input, expect);
