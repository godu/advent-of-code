namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day16 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day16 (input: string, expect: string) =
        Assert.AreEqual(Day16.answer input, expect);
