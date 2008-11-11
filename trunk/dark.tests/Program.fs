#light

open System
open FsUnit
open dark





let isEven x = x % 2 = 0

specs "isEven Specs" [
    spec "0 should be even."
        (isEven 0 |> should be True)
    spec "1 should not be even."
        (isEven 1 |> should be False)
    spec "2 should be even."
        (isEven 2 |> should be True)
    spec "-1 should not be even."
        (isEven (-1) |> should be False)
    spec "-2 should be even."
        (isEven (-2) |> should be True)
]

printfn "%s" (Results.summary())

Console.ReadKey() |> ignore

