module Permutation exposing
    ( Permutation(..)
    , generate
    , identity
    , showCycles
    , toCycles
    )

import Array exposing (Array)
import List.Extra as List
import Random
import Random.List


{-| Represents permutation of {1..n}

Internally represented as Array {0..n-1}

-}
type Permutation
    = Permutation (Array Int)


identity : Int -> Permutation
identity n =
    Permutation <| Array.initialize n Basics.identity


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


generate : Int -> Cmd Permutation
generate n =
    List.range 0 (n - 1)
        |> Random.List.shuffle
        |> Random.generate (Array.fromList >> Permutation)
