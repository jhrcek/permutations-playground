module Permutation exposing (Permutation(..), identity, showCycles, toCycles)

import Array exposing (Array)
import List.Extra as List


type Permutation
    = Permutation (Array Int)


identity : Int -> Permutation
identity n =
    Permutation <| Array.initialize n (\i -> i)


showCycles : Permutation -> String
showCycles p =
    let
        cs =
            toCycles p
    in
    if List.all (\c -> List.length c == 1) cs then
        "()"

    else
        String.concat <|
            List.map (\cyc -> "(" ++ String.join " " (List.map String.fromInt cyc) ++ ")") cs


toCycles : Permutation -> List (List Int)
toCycles ((Permutation perm) as p) =
    let
        unrollCycle : Int -> List Int
        unrollCycle start =
            start
                :: List.unfoldr
                    (\i ->
                        Array.get i perm
                            |> Maybe.andThen
                                (\j ->
                                    if j /= start then
                                        Just ( j, j )

                                    else
                                        Nothing
                                )
                    )
                    start

        f n cs =
            if List.any (List.member n) cs then
                cs

            else
                unrollCycle n :: cs
    in
    List.range 0 (size p - 1)
        |> List.foldl f []
        |> List.reverse


size : Permutation -> Int
size (Permutation perm) =
    Array.length perm
