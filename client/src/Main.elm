module Main exposing (main)

import Array
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import List.Extra as List
import Permutation exposing (Permutation(..))
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init 3
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
    , editState : EditState
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
    | AddIdentityLast
    | RemoveLastPermutation
    | GeneratePermutation Int
    | ResetPermutation Int
    | InvertPermutation Int
    | ShiftPermutationLeft Int
    | ShiftPermutationRight Int
    | EditPermutation Int Permutation
    | CancelEdit
    | SaveEdit Int Permutation
    | SetPermutationString String
    | RemovePermutation Int
    | SetPermutation Int Permutation
    | SetPermutations (List Permutation)
    | GenerateAll
    | ResetAll
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


type EditState
    = NotEditing
    | Editing Int String (Result String Permutation)


defaultImage : CanvasImage
defaultImage =
    { horizontalDist = 100
    , verticalDist = 100
    , circleRadius = 15
    , paddingX = 100
    , paddingY = 100
    }


init : Int -> () -> ( Model, Cmd Msg )
init n () =
    ( { movingCircle = Nothing
      , circle = ( 100, 100 )
      , permutations =
            [ Permutation (Array.fromList [ 1, 2, 0 ])
            , Permutation (Array.fromList [ 2, 0, 1 ])
            , Permutation.identity n
            ]
      , n = n
      , viewPort = { width = 1024, height = 768 }
      , canvasImage = defaultImage
      , editState = NotEditing
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
    case msg of
        StartAt point ->
            pure { model | movingCircle = getCircleAt point model }

        MoveAt ( _, y ) ->
            pure
                (case model.movingCircle of
                    Just ( x, _ ) ->
                        { model | movingCircle = Just ( x, y ) }

                    Nothing ->
                        model
                )

        EndAt ( _, y ) ->
            pure
                (case model.movingCircle of
                    Just ( x, _ ) ->
                        { model | movingCircle = Nothing, circle = ( x, roundToNearest100 y ) }

                    Nothing ->
                        model
                )

        GotViewport viewPort ->
            pure { model | viewPort = viewPort }

        SetHorizontalDist newDomCodDist ->
            pure (updateImage (\image -> { image | horizontalDist = newDomCodDist }) model)

        SetVerticalDist newVerticalDist ->
            pure (updateImage (\image -> { image | verticalDist = newVerticalDist }) model)

        SetCircleRadius newCircleRadius ->
            pure (updateImage (\image -> { image | circleRadius = newCircleRadius }) model)

        SetPaddingX newPaddingX ->
            pure (updateImage (\image -> { image | paddingX = newPaddingX }) model)

        SetPaddingY newPaddingY ->
            pure (updateImage (\image -> { image | paddingY = newPaddingY }) model)

        SetN newN ->
            pure { model | n = newN, permutations = List.map (\_ -> Permutation.identity newN) model.permutations }

        AddIdentityLast ->
            pure { model | permutations = model.permutations ++ [ Permutation.identity model.n ] }

        RemoveLastPermutation ->
            pure { model | permutations = List.take (List.length model.permutations - 1) model.permutations }

        GeneratePermutation i ->
            ( model, Cmd.map (SetPermutation i) (Permutation.generate model.n) )

        ResetPermutation i ->
            pure { model | permutations = List.setAt i (Permutation.identity model.n) model.permutations }

        InvertPermutation i ->
            pure { model | permutations = List.updateAt i Permutation.inverse model.permutations }

        ShiftPermutationLeft i ->
            case
                Maybe.map2
                    (\a b ->
                        let
                            ( aNew, bNew ) =
                                Permutation.shiftRightToLeft a b
                        in
                        model.permutations
                            |> List.setAt (i - 1) aNew
                            |> List.setAt i bNew
                    )
                    (List.getAt (i - 1) model.permutations)
                    (List.getAt i model.permutations)
            of
                Just newPermutations ->
                    pure { model | permutations = newPermutations }

                Nothing ->
                    pure model

        ShiftPermutationRight i ->
            case
                Maybe.map2
                    (\a b ->
                        let
                            ( aNew, bNew ) =
                                Permutation.shiftLeftToRight a b
                        in
                        model.permutations
                            |> List.setAt i aNew
                            |> List.setAt (i + 1) bNew
                    )
                    (List.getAt i model.permutations)
                    (List.getAt (i + 1) model.permutations)
            of
                Just newPermutations ->
                    pure { model | permutations = newPermutations }

                Nothing ->
                    pure model

        SetPermutation i perm ->
            pure { model | permutations = List.setAt i perm model.permutations }

        GenerateAll ->
            ( model, Cmd.map SetPermutations (Permutation.generateMany model.n (List.length model.permutations)) )

        SetPermutations ps ->
            pure { model | permutations = ps }

        ResetAll ->
            pure { model | permutations = List.map (\_ -> Permutation.identity model.n) model.permutations }

        RemovePermutation i ->
            pure { model | permutations = List.removeAt i model.permutations }

        EditPermutation i perm ->
            let
                cycles =
                    Permutation.showCycles perm
            in
            ( { model | editState = Editing i cycles (Permutation.parseCycles model.n cycles) }
            , focusPermutationInput
            )

        SetPermutationString str ->
            let
                newEditState =
                    case model.editState of
                        NotEditing ->
                            NotEditing

                        Editing i _ _ ->
                            Editing i str (Permutation.parseCycles model.n str)
            in
            pure { model | editState = newEditState }

        CancelEdit ->
            pure { model | editState = NotEditing }

        SaveEdit i perm ->
            pure { model | permutations = List.setAt i perm model.permutations, editState = NotEditing }

        NoOp ->
            pure model


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


updateImage : (CanvasImage -> CanvasImage) -> Model -> Model
updateImage f model =
    { model | canvasImage = f model.canvasImage }


roundToNearest100 : Float -> Float
roundToNearest100 x =
    100 * toFloat (round (x / 100))


view : Model -> Html Msg
view { permutations, viewPort, n, canvasImage, editState } =
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "300px auto"
        ]
        [ imageConfigControls canvasImage n permutations editState
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


imageConfigControls : CanvasImage -> Int -> List Permutation -> EditState -> Html Msg
imageConfigControls canvasImage n permutations editState =
    let
        permCount =
            List.length permutations
    in
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
            [ Html.label []
                [ Html.text "Set size "
                , Html.input
                    [ HA.type_ "number"
                    , HA.min "1"
                    , HA.max "50"
                    , HA.value (String.fromInt n)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map (SetN << clamp 1 50) << String.toInt)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.button [ HE.onClick RemoveLastPermutation ] [ Html.text "Remove last" ]
            , Html.button [ HE.onClick AddIdentityLast ] [ Html.text "Add last" ]
            ]
        , Html.div []
            [ Html.button [ HE.onClick GenerateAll ] [ Html.text "Gen all" ]
            , Html.button [ HE.onClick ResetAll ] [ Html.text "Reset all" ]
            ]
        , Html.div [] <|
            List.indexedMap
                (\i p -> viewPermutation i p editState permCount)
                permutations
        , let
            composition =
                List.foldr Permutation.compose (Permutation.identity n) permutations
          in
          Html.div []
            [ Html.div []
                [ Html.text "All composed: "
                , Html.br [] []
                , Html.text <| Permutation.showCycles composition
                ]
            , Html.div []
                [ Html.text "Fixed points: "
                , Html.br [] []
                , Html.text <|
                    String.join ", " <|
                        List.map String.fromInt <|
                            Permutation.fixedPoints composition
                ]
            ]
        ]


viewPermutation : Int -> Permutation -> EditState -> Int -> Html Msg
viewPermutation index perm editState permCount =
    case editState of
        NotEditing ->
            viewPermutationPlain index permCount perm

        Editing editedIndex cyclesStr parseRes ->
            if index == editedIndex then
                let
                    cancelButton =
                        Html.button [ HE.onClick CancelEdit ] [ Html.text "Cancel" ]

                    inputAttrs =
                        [ HA.value cyclesStr
                        , HE.onInput SetPermutationString
                        , HA.id permutationInputId
                        ]
                in
                case parseRes of
                    Err err ->
                        Html.div []
                            [ Html.input inputAttrs []
                            , cancelButton
                            , Html.div [ HA.style "color" "red" ] [ Html.text err ]
                            ]

                    Ok newPerm ->
                        Html.div []
                            [ Html.input (onEnter (SaveEdit index newPerm) :: inputAttrs) []
                            , cancelButton
                            , Html.button
                                [ HE.onClick (SaveEdit index newPerm) ]
                                [ Html.text "Save" ]
                            ]

            else
                viewPermutationPlain index permCount perm


permutationInputId : String
permutationInputId =
    "perm-input"


focusPermutationInput : Cmd Msg
focusPermutationInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus permutationInputId)


onEnter : msg -> Attribute msg
onEnter msg =
    HE.on "keydown"
        (HE.keyCode
            |> Decode.andThen
                (\keyCode ->
                    if keyCode == 13 {- Enter -} then
                        Decode.succeed msg

                    else
                        Decode.fail ""
                )
        )


viewPermutationPlain : Int -> Int -> Permutation -> Html Msg
viewPermutationPlain index permCount perm =
    Html.div []
        [ Html.button [ HE.onClick (ResetPermutation index), HA.title "Reset to identity" ] [ Html.text "↺" ]
        , Html.button [ HE.onClick (GeneratePermutation index), HA.title "Generate random permutation" ] [ Html.text "⚄" ]
        , Html.button [ HE.onClick (InvertPermutation index), HA.title "Invert" ] [ Html.text "🠔" ]
        , Html.button
            [ if index > 0 then
                HE.onClick (ShiftPermutationLeft index)

              else
                HA.disabled True
            , HA.title "Shift left without affecting composition"
            ]
            [ Html.text "«" ]
        , Html.button
            [ if index < (permCount - 1) then
                HE.onClick (ShiftPermutationRight index)

              else
                HA.disabled True
            , HA.title "Shift right without affecting composition"
            ]
            [ Html.text "»" ]
        , Html.button [ HE.onClick (EditPermutation index perm), HA.title "Edit permutation" ] [ Html.text "🖉" ]
        , Html.button [ HE.onClick (RemovePermutation index), HA.title "Delete permutation" ] [ Html.text "✕" ]
        , Html.text <| Permutation.showCycles perm
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotViewport { width = w, height = h })



-- TODO add a way to edit permutation without altering composition
-- TODO CSS: align controls and buttons in one column
-- TODO add way to save particular permutation
-- TODO add hard-wired model with pre-saved rubik's cube generating permutations
-- TODO allow viewing permutations with 50 sized set
