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
      , permutations = []
      , circleRadius = 20
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
view { circle, movingCircle, circleRadius } =
    Html.div []
        [ Canvas.toHtml ( w, h )
            [ Mouse.onDown (.offsetPos >> StartAt)
            , Mouse.onMove (.offsetPos >> MoveAt)
            , Mouse.onUp (.offsetPos >> EndAt)
            ]
            [ Canvas.clear ( 0, 0 ) w h
            , Canvas.shapes
                [ fill Color.white, stroke Color.black ]
                [ Canvas.path (Maybe.withDefault circle movingCircle)
                    [ Canvas.lineTo ( 200, 400 ), Canvas.lineTo ( 400, 200 ) ]
                ]
            , Canvas.shapes
                [ fill Color.white, stroke Color.black ]
                [ Canvas.circle (Maybe.withDefault circle movingCircle) circleRadius
                , Canvas.circle ( 100, 200 ) circleRadius
                , Canvas.circle ( 100, 300 ) circleRadius
                , Canvas.circle ( 100, 400 ) circleRadius
                ]
            , textAt (Maybe.withDefault circle movingCircle) "1"
            , textAt ( 100, 200 ) "2"
            , textAt ( 100, 300 ) "3"
            , textAt ( 100, 400 ) "4"
            ]
        , Html.div []
            []
        ]


textAt : Point -> String -> Renderable
textAt =
    Canvas.text
        [ font { size = 16, family = "sans-serif" }, align Center, baseLine Middle ]


pointDistance : Point -> Point -> Float
pointDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


getCircleAt : Point -> Model -> Maybe Point
getCircleAt p { circle, circleRadius } =
    if pointDistance circle p <= circleRadius then
        Just circle

    else
        Nothing


type alias Permutation =
    Array Int
