module Permutation exposing
    ( Permutation(..)
    , compose
    , generate
    , generateMany
    , identity
    , inverse
    , parseCycles
    , shiftLeftToRight
    , shiftRightToLeft
    , showCycles
    )

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as List
import Parser as P exposing ((|.), Parser, Problem(..))
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


{-| shiftLeftToRight a b returns a pair of permutations (a;b;a^-1, a).
The effect of this is that a is "shifted to the right", while the pair still composes to a;b
-}
shiftLeftToRight : Permutation -> Permutation -> ( Permutation, Permutation )
shiftLeftToRight a b =
    ( compose (compose a b) (inverse a), a )


{-| shiftRightToLeft a b returns a pair of permutations (b, b^-1;a;b).
The effect of this is that a is "shifted to the left", while the pair still composes to a;b
-}
shiftRightToLeft : Permutation -> Permutation -> ( Permutation, Permutation )
shiftRightToLeft a b =
    ( b, compose (compose (inverse b) a) b )


inverse : Permutation -> Permutation
inverse (Permutation p) =
    Array.toIndexedList p
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> Array.fromList
        |> Permutation


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


parseCycles : Int -> String -> Result String Permutation
parseCycles n input =
    P.run (parser n) input
        |> Result.mapError
            (\listDeadEnds ->
                List.map showDeadEnd listDeadEnds
                    |> String.join ";"
            )


showDeadEnd : P.DeadEnd -> String
showDeadEnd { problem } =
    case problem of
        Expecting str ->
            "Expecting '" ++ str ++ "'"

        ExpectingInt ->
            "Expecting Int"

        ExpectingHex ->
            "Expecting Hex"

        ExpectingOctal ->
            "Expecting Octal"

        ExpectingBinary ->
            "Expecting Binary"

        ExpectingFloat ->
            "Expecting Float"

        ExpectingNumber ->
            "Expecting Number"

        ExpectingVariable ->
            "Expecting Variable"

        ExpectingSymbol str ->
            "Expecting symbol " ++ str

        ExpectingKeyword str ->
            "Expecting keyword" ++ str

        ExpectingEnd ->
            "Expecting End"

        UnexpectedChar ->
            "Unexpected Char"

        Problem str ->
            str

        BadRepeat ->
            "Bad repeat"


parser : Int -> Parser Permutation
parser n =
    P.oneOf
        [ P.succeed (identity n)
            |. P.keyword "()"
        , P.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = P.spaces
            , item = cycleParser n
            , trailing = P.Forbidden
            }
            |> P.andThen
                (\cycles ->
                    case cycles of
                        [] ->
                            P.succeed (identity n)

                        xs ->
                            if List.allDifferent (List.concat xs) then
                                P.succeed (fromCycles n cycles)

                            else
                                P.problem "One or more numbers repeated within some cycles"
                )
        ]


{-| This assumes `List (List Int)` represents valid permutation cycles from the parser
-}
fromCycles : Int -> List (List Int) -> Permutation
fromCycles n cycles =
    let
        addMappingsFromCycle : List Int -> Dict Int Int -> Dict Int Int
        addMappingsFromCycle cycle =
            let
                addLastToFirst : Dict Int Int -> Dict Int Int
                addLastToFirst =
                    Maybe.map2 (\lst fst -> Dict.insert lst fst) (List.last cycle) (List.head cycle)
                        |> Maybe.withDefault Basics.identity

                addPairs : List Int -> Dict Int Int -> Dict Int Int
                addPairs c dict =
                    case c of
                        x :: y :: zs ->
                            addPairs (y :: zs) (Dict.insert x y dict)

                        [ _ ] ->
                            dict

                        [] ->
                            dict
            in
            addPairs cycle << addLastToFirst

        mappingsDict =
            List.foldl addMappingsFromCycle Dict.empty cycles
    in
    Permutation
        (Array.initialize n
            (\i ->
                Dict.get (i + 1) mappingsDict
                    |> Maybe.map (\j -> j - 1)
                    |> Maybe.withDefault i
            )
        )


cycleParser : Int -> Parser (List Int)
cycleParser n =
    P.sequence
        { start = "("
        , separator = " "
        , end = ")"
        , spaces = P.succeed ()
        , item = P.int
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\ints ->
                if List.all (\x -> 1 <= x && x <= n) ints then
                    if List.allDifferent ints then
                        P.succeed ints

                    else
                        P.problem "The numbers within each cycle must be unique"

                else
                    P.problem ("The numbers have to be between 1 and " ++ String.fromInt n ++ " inclusive")
            )
