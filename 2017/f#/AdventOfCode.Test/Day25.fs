namespace AdventOfCode.Test

open Microsoft.VisualStudio.TestTools.UnitTesting;
open AdventOfCode;

[<TestClass>]
type Day25 () =
    [<DataTestMethod>]
    [<DataRow("FOO", "FOO")>]
    member this.Day25 (input: string, expect: string) =
        Assert.AreEqual(Day25.answer input, expect);
