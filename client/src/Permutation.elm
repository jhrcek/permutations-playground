module Permutation exposing
    ( Permutation(..)
    , compose
    , generate
    , generateMany
    , identity
    , showCycles
    , toCycles
    )

import Array exposing (Array)
import List.Extra as List
import Random exposing (Generator)
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
            List.map (\cyc -> "(" ++ String.join " " (List.map (String.fromInt << (+) 1) cyc) ++ ")") cs


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


{-| composition from left to right
p1 ; p2

The result of `compose p1 p2` is a permutation which is like applying p1 followed by p2

-}
compose : Permutation -> Permutation -> Permutation
compose (Permutation p1) (Permutation p2) =
    Permutation (Array.map (\i -> Array.get i p2 |> Maybe.withDefault 0) p1)


generateOne : Int -> Generator Permutation
generateOne setSize =
    List.range 0 (setSize - 1)
        |> Random.List.shuffle
        |> Random.map (Array.fromList >> Permutation)


generate : Int -> Cmd Permutation
generate n =
    Random.generate Basics.identity (generateOne n)


{-| generateMany setSize listLength
-}
generateMany : Int -> Int -> Cmd (List Permutation)
generateMany setSize listLength =
    Random.list listLength (generateOne setSize)
        |> Random.generate Basics.identity
