module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { movingCircle : Maybe Point
    , circle : Point
    , permutations : List Permutation
    , n : Int
    , viewPort : ViewPort
    , canvasImage : CanvasImage
    }


type Msg
    = StartAt Point
    | MoveAt Point
    | EndAt Point
    | GotViewport ViewPort
    | SetHorizontalDist Int
    | SetVerticalDist Int
    | SetCircleRadius Float
    | SetPaddingX Float
    | SetPaddingY Float
    | SetN Int
    | AddLastPermutation
    | RemoveLastPermutation
    | NoOp


type alias ViewPort =
    { width : Int
    , height : Int
    }


type alias CanvasImage =
    { horizontalDist : Int
    , verticalDist : Int
    , circleRadius : Float
    , paddingX : Float
    , paddingY : Float
    }


defaultImage : CanvasImage
defaultImage =
    { horizontalDist = 100
    , verticalDist = 100
    , circleRadius = 15
    , paddingX = 100
    , paddingY = 100
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { movingCircle = Nothing
      , circle = ( 100, 100 )
      , permutations =
            [ identityPermutation 4
            , identityPermutation 4
            , identityPermutation 4
            , identityPermutation 4
            ]
      , n = 4
      , viewPort = { width = 1024, height = 768 }
      , canvasImage = defaultImage
      }
    , Task.perform
        (\vp ->
            GotViewport
                { width = round vp.viewport.width
                , height = round vp.viewport.height
                }
        )
        getViewport
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

        GotViewport viewPort ->
            { model | viewPort = viewPort }

        SetHorizontalDist newDomCodDist ->
            updateImage (\image -> { image | horizontalDist = newDomCodDist }) model

        SetVerticalDist newVerticalDist ->
            updateImage (\image -> { image | verticalDist = newVerticalDist }) model

        SetCircleRadius newCircleRadius ->
            updateImage (\image -> { image | circleRadius = newCircleRadius }) model

        SetPaddingX newPaddingX ->
            updateImage (\image -> { image | paddingX = newPaddingX }) model

        SetPaddingY newPaddingY ->
            updateImage (\image -> { image | paddingY = newPaddingY }) model

        SetN newN ->
            { model | n = newN, permutations = List.map (\_ -> identityPermutation newN) model.permutations }

        AddLastPermutation ->
            { model | permutations = model.permutations ++ [ identityPermutation model.n ] }

        RemoveLastPermutation ->
            { model | permutations = List.take (List.length model.permutations - 1) model.permutations }

        NoOp ->
            model
    , Cmd.none
    )


updateImage : (CanvasImage -> CanvasImage) -> Model -> Model
updateImage f model =
    { model | canvasImage = f model.canvasImage }


roundToNearest100 : Float -> Float
roundToNearest100 x =
    100 * toFloat (round (x / 100))


view : Model -> Html Msg
view { circle, movingCircle, permutations, viewPort, n, canvasImage } =
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "300px auto"
        ]
        [ imageConfigControls canvasImage n
        , Canvas.toHtml ( viewPort.width - 300, viewPort.height )
            [ Mouse.onDown (.offsetPos >> StartAt)
            , Mouse.onMove (.offsetPos >> MoveAt)
            , Mouse.onUp (.offsetPos >> EndAt)
            ]
            (Canvas.clear ( 0, 0 ) (toFloat viewPort.width) (toFloat viewPort.height)
                :: permutationLines canvasImage n permutations
                :: permutationCircles canvasImage n permutations
                :: permutationTexts canvasImage n permutations
            )
        ]


imageConfigControls : CanvasImage -> Int -> Html Msg
imageConfigControls canvasImage n =
    Html.div []
        [ Html.h3 [] [ Html.text "Controls" ]
        , Html.div []
            [ Html.label []
                [ Html.text "horizontal"
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "20"
                    , HA.max "200"
                    , HA.value (String.fromInt canvasImage.horizontalDist)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map SetHorizontalDist << String.toInt)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label []
                [ Html.text "vertical"
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "20"
                    , HA.max "200"
                    , HA.value (String.fromInt canvasImage.verticalDist)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map SetVerticalDist << String.toInt)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label []
                [ Html.text "radius"
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "10"
                    , HA.max "50"
                    , HA.value (String.fromFloat canvasImage.circleRadius)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map SetCircleRadius << String.toFloat)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label []
                [ Html.text "padding X"
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "10"
                    , HA.max "200"
                    , HA.value (String.fromFloat canvasImage.paddingX)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map SetPaddingX << String.toFloat)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label []
                [ Html.text "padding Y"
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "10"
                    , HA.max "200"
                    , HA.value (String.fromFloat canvasImage.paddingY)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map SetPaddingY << String.toFloat)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.text "Set size "
            , Html.button [ HE.onClick <| SetN <| clamp 1 50 <| n - 1 ] [ Html.text "-1" ]
            , Html.text (" " ++ String.fromInt n ++ " ")
            , Html.button [ HE.onClick <| SetN <| clamp 1 50 <| n + 1 ] [ Html.text "+1" ]
            ]
        , Html.div []
            [ Html.button [ HE.onClick RemoveLastPermutation ] [ Html.text "Remove perm" ]
            , Html.button [ HE.onClick AddLastPermutation ] [ Html.text "Add perm" ]
            ]
        ]


permutationLines : CanvasImage -> Int -> List Permutation -> Renderable
permutationLines ci n permutations =
    Canvas.shapes
        [ fill Color.white, stroke Color.black ]
        (List.map
            (\i ->
                Canvas.path ( ci.paddingX, ci.paddingY + toFloat (ci.verticalDist * i) )
                    (List.foldl
                        (\(Permutation p) ( curX, curXDim, moves ) ->
                            let
                                nextX =
                                    Maybe.withDefault 0 (Array.get curX p)
                            in
                            ( nextX
                            , curXDim + ci.horizontalDist
                            , moves ++ [ Canvas.lineTo ( toFloat curXDim, toFloat (round ci.paddingY + ci.verticalDist * nextX) ) ]
                            )
                        )
                        ( i, round ci.paddingX + ci.horizontalDist, [] )
                        permutations
                        |> (\( _, _, moves ) -> moves)
                    )
            )
            (List.range 0 (n - 1))
        )


permutationCircles : CanvasImage -> Int -> List Permutation -> Renderable
permutationCircles ci n permutations =
    Canvas.shapes
        [ fill Color.white, stroke Color.black ]
        (List.range 0 (List.length permutations)
            |> List.concatMap
                (\colIdx ->
                    List.range 0 (n - 1)
                        |> List.map
                            (\rowIdx ->
                                Canvas.circle
                                    ( ci.paddingX + toFloat (colIdx * ci.horizontalDist)
                                    , ci.paddingY + toFloat (rowIdx * ci.verticalDist)
                                    )
                                    ci.circleRadius
                            )
                )
        )


permutationTexts : CanvasImage -> Int -> List Permutation -> List Renderable
permutationTexts ci n permutations =
    List.range 0 (List.length permutations)
        |> List.concatMap
            (\col ->
                List.range 0 (n - 1)
                    |> List.map
                        (\row ->
                            textAt
                                ( ci.paddingX + toFloat (col * ci.horizontalDist)
                                , ci.paddingY + toFloat (row * ci.verticalDist)
                                )
                                (String.fromInt (row + 1))
                        )
            )


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
getCircleAt p { circle, canvasImage } =
    if pointDistance circle p <= canvasImage.circleRadius then
        Just circle

    else
        Nothing


type Permutation
    = Permutation (Array Int)


identityPermutation : Int -> Permutation
identityPermutation n =
    Permutation (Array.fromList (List.range 0 (n - 1)))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotViewport { width = w, height = h })



-- TODO add a way to input permutations - via cycle notation (1 2 3) (4 5)?
-- TODO add parser of cycleNotations `parseCycles : Int -> String -> Maybe Permutation`
-- TODO add showCycles : Permutation -> String
-- TODO add a way to edit permutation independently of others
-- TODO add a way to edit permutation without altering composition
-- TODO add a way to move given permutation left-right within composition without altering the composition
-- that is given 1) a ; p = b find c such that p ; c = b
--               2) p ; a = b find c such that c ; p = b
-- TODO add composeLeftToRight : Permutation -> Permutation -> Permutation
-- TODO add ability to generate random permutation in each spot
