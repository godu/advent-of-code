namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day19 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day19 (input: string, expect: string) =
        Assert.AreEqual(Day19.answer input, expect);
