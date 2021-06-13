module Main exposing (main)

import Array exposing (Array)
import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


{-| TODO make this fullscreen-ish and dynamic on window resize
-}
h : number
h =
    900


w : number
w =
    900


type alias Model =
    { movingCircle : Maybe Point
    , circle : Point
    , permutations : List Permutation
    , n : Int
    , circleRadius : Float
    }


type Msg
    = StartAt Point
    | MoveAt Point
    | EndAt Point


init : () -> ( Model, Cmd Msg )
init () =
    ( { movingCircle = Nothing
      , circle = ( 100, 100 )
      , permutations =
            [ identityPermutation 4
            , identityPermutation 4
            ]
      , n = 4
      , circleRadius = 15
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        StartAt point ->
            { model | movingCircle = getCircleAt point model }

        MoveAt ( _, y ) ->
            case model.movingCircle of
                Just ( x, _ ) ->
                    { model | movingCircle = Just ( x, y ) }

                Nothing ->
                    model

        EndAt ( _, y ) ->
            case model.movingCircle of
                Just ( x, _ ) ->
                    { model | movingCircle = Nothing, circle = ( x, roundToNearest100 y ) }

                Nothing ->
                    model
    , Cmd.none
    )


roundToNearest100 : Float -> Float
roundToNearest100 x =
    100 * toFloat (round (x / 100))


view : Model -> Html Msg
view { circle, movingCircle, circleRadius, permutations, n } =
    Html.div []
        [ Canvas.toHtml ( w, h )
            [ Mouse.onDown (.offsetPos >> StartAt)
            , Mouse.onMove (.offsetPos >> MoveAt)
            , Mouse.onUp (.offsetPos >> EndAt)
            ]
            (Canvas.clear ( 0, 0 ) w h
                :: permutationLines n permutations
                :: permutationCircles n circleRadius permutations
                :: permutationTexts n permutations
            )
        , Html.div []
            []
        ]


permutationLines : Int -> List Permutation -> Renderable
permutationLines n permutations =
    Canvas.shapes
        [ fill Color.white, stroke Color.black ]
        (List.map
            (\i ->
                Canvas.path ( paddingX, paddingY + x1x2Dist * toFloat i )
                    (List.foldl
                        (\(Permutation p) ( curX, curXDim, moves ) ->
                            let
                                nextX =
                                    Maybe.withDefault 0 (Array.get curX p)
                            in
                            ( nextX
                            , curXDim + domCodDist
                            , moves ++ [ Canvas.lineTo ( curXDim, paddingY + x1x2Dist * toFloat nextX ) ]
                            )
                        )
                        ( i, paddingX + domCodDist, [] )
                        permutations
                        |> (\( _, _, moves ) -> moves)
                    )
            )
            (List.range 0 (n - 1))
        )


permutationCircles : Int -> Float -> List Permutation -> Renderable
permutationCircles n circleRadius permutations =
    Canvas.shapes
        [ fill Color.white, stroke Color.black ]
        (List.range 0 (List.length permutations)
            |> List.concatMap
                (\colIdx ->
                    List.range 0 (n - 1)
                        |> List.map
                            (\rowIdx ->
                                Canvas.circle
                                    ( paddingX + toFloat colIdx * domCodDist
                                    , paddingY + toFloat rowIdx * x1x2Dist
                                    )
                                    circleRadius
                            )
                )
        )


permutationTexts : Int -> List Permutation -> List Renderable
permutationTexts n permutations =
    List.range 0 (List.length permutations)
        |> List.concatMap
            (\col ->
                List.range 0 (n - 1)
                    |> List.map
                        (\row ->
                            textAt
                                ( paddingX + toFloat col * domCodDist
                                , paddingY + toFloat row * x1x2Dist
                                )
                                (String.fromInt (col + 1))
                        )
            )


x1x2Dist =
    100


paddingX =
    100


paddingY =
    100


domCodDist =
    100


textAt : Point -> String -> Renderable
textAt =
    Canvas.text
        [ font { size = 16, family = "sans-serif" }
        , align Center
        , baseLine Middle
        ]


pointDistance : Point -> Point -> Float
pointDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


getCircleAt : Point -> Model -> Maybe Point
getCircleAt p { circle, circleRadius } =
    if pointDistance circle p <= circleRadius then
        Just circle

    else
        Nothing


type Permutation
    = Permutation (Array Int)


identityPermutation : Int -> Permutation
identityPermutation n =
    Permutation (Array.fromList (List.range 0 (n - 1)))



-- TODO add a way to input permutations - via cycle notation (1 2 3) (4 5)?
-- TODO add parser of cycleNotations `parseCycles : Int -> String -> Maybe Permutation`
-- TODO add showCycles : Permutation -> String
-- TODO add a way to edit view params (circle radius, domain-codomain distance, x1-x2 distance, x/y padding(?))
-- TODO add a way to increase/decrease the number of perms being displayed
-- TODO add a way to increase/decrease n in Sn
-- TODO add a way to edit permutation independently of others
-- TODO add a way to edit permutation without altering composition
-- TODO add a way to move given permutation left-right within composition without altering the composition
-- that is given 1) a ; p = b find c such that p ; c = b
--               2) p ; a = b find c such taht c ; p = b
-- TODO add composeLeftToRight : Permutation -> Permutation -> Permutation
-- TODO add ability to generate random permutation in each spot
