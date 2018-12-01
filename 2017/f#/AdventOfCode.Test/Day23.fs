namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day23 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day23 (input: string, expect: string) =
        Assert.AreEqual(Day23.answer input, expect);
