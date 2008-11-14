#light

open System
open FsUnit
open dark
open dark_tests

let tests = List.concat [ tile_tests.tests();
                          rectangle_tests.tests();
                          map_tests.tests() ]

specs "dark tests" tests
printfn "%s" (Results.summary())


Console.ReadKey() |> ignore

