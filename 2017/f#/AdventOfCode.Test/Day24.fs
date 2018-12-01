namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day24 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day24 (input: string, expect: string) =
        Assert.AreEqual(Day24.answer input, expect);
