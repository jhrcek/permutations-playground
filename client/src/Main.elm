module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color
import Dict exposing (Dict)
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
    , savedPermutations : Dict String Permutation
    , setSize : Int
    , canvasImage : CanvasImage
    , permutationEdit : EditState
    }


type Msg
    = StartAt Point
    | MoveAt Point
    | EndAt Point
    | SetHorizontalDist Int
    | SetVerticalDist Int
    | SetCircleRadius Float
    | SetPaddingX Float
    | SetPaddingY Float
    | SetN Int
    | AddIdentityLast
    | RemoveLastPermutation
    | UpdateSaved String PermutationUpdate
    | UpdateComposed Int PermutationUpdate
    | StartNameEdit String
    | SaveNameEdit
    | SetPermutationName String
    | ShiftPermutationLeft Int
    | ShiftPermutationRight Int
    | CancelEdit
    | SetPermutationString String
    | SetPermutations (List Permutation)
    | GenerateAll
    | ResetAll
    | NoOp


type PermutationUpdate
    = ResetPermutation
    | GeneratePermutation
    | InvertPermutation
    | StartPermutationEdit Permutation
    | RemovePermutation
    | SetPermutation Permutation
    | SaveEditedPermutation Permutation


type alias CanvasImage =
    { horizontalDist : Int
    , verticalDist : Int
    , circleRadius : Float
    , paddingX : Float
    , paddingY : Float
    }


type EditState
    = NotEditing
    | EditingPermutation PermutationLocation String (Result String Permutation)
    | EditingSavedName String String


type PermutationLocation
    = Composed Int -- Index within the composition
    | Saved String -- Name within saved permutations map


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
      , savedPermutations =
            Dict.fromList
                [ ( "a", Permutation (Array.fromList [ 1, 2, 0 ]) )
                , ( "b", Permutation (Array.fromList [ 2, 0, 1 ]) )
                ]
      , setSize = n
      , canvasImage = defaultImage
      , permutationEdit = NotEditing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pure model

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
            pure
                { model
                    | setSize = newN
                    , permutations = List.map (\_ -> Permutation.identity newN) model.permutations
                    , savedPermutations = Dict.empty
                }

        AddIdentityLast ->
            pure { model | permutations = model.permutations ++ [ Permutation.identity model.setSize ] }

        RemoveLastPermutation ->
            pure { model | permutations = List.take (List.length model.permutations - 1) model.permutations }

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

        GenerateAll ->
            ( model, Cmd.map SetPermutations (Permutation.generateMany model.setSize (List.length model.permutations)) )

        SetPermutations ps ->
            pure { model | permutations = ps }

        ResetAll ->
            pure { model | permutations = List.map (\_ -> Permutation.identity model.setSize) model.permutations }

        SetPermutationString str ->
            let
                newEditState =
                    case model.permutationEdit of
                        NotEditing ->
                            NotEditing

                        EditingPermutation permLocation _ _ ->
                            EditingPermutation permLocation str (Permutation.parseCycles model.setSize str)

                        EditingSavedName oldName newName ->
                            EditingSavedName oldName newName
            in
            pure { model | permutationEdit = newEditState }

        CancelEdit ->
            pure { model | permutationEdit = NotEditing }

        StartNameEdit originalName ->
            ( { model | permutationEdit = EditingSavedName originalName "" }
            , focusPermutationInput
            )

        UpdateSaved permName permutationUpdate ->
            case permutationUpdate of
                ResetPermutation ->
                    pure { model | savedPermutations = Dict.update permName (Maybe.map (always (Permutation.identity model.setSize))) model.savedPermutations }

                GeneratePermutation ->
                    ( model, Cmd.map (UpdateSaved permName << SetPermutation) (Permutation.generate model.setSize) )

                InvertPermutation ->
                    pure { model | savedPermutations = Dict.update permName (Maybe.map Permutation.inverse) model.savedPermutations }

                StartPermutationEdit perm ->
                    let
                        cycles =
                            Permutation.showCycles perm
                    in
                    ( { model | permutationEdit = EditingPermutation (Saved permName) cycles (Permutation.parseCycles model.setSize cycles) }
                    , focusPermutationInput
                    )

                RemovePermutation ->
                    pure { model | savedPermutations = Dict.remove permName model.savedPermutations }

                SetPermutation perm ->
                    pure { model | savedPermutations = Dict.insert permName perm model.savedPermutations }

                SaveEditedPermutation perm ->
                    pure
                        { model
                            | savedPermutations = Dict.insert permName perm model.savedPermutations
                            , permutationEdit = NotEditing
                        }

        UpdateComposed i permutationUpdate ->
            case permutationUpdate of
                ResetPermutation ->
                    pure { model | permutations = List.setAt i (Permutation.identity model.setSize) model.permutations }

                GeneratePermutation ->
                    ( model, Cmd.map (UpdateComposed i << SetPermutation) (Permutation.generate model.setSize) )

                InvertPermutation ->
                    pure { model | permutations = List.updateAt i Permutation.inverse model.permutations }

                StartPermutationEdit perm ->
                    let
                        cycles =
                            Permutation.showCycles perm
                    in
                    ( { model | permutationEdit = EditingPermutation (Composed i) cycles (Permutation.parseCycles model.setSize cycles) }
                    , focusPermutationInput
                    )

                RemovePermutation ->
                    pure { model | permutations = List.removeAt i model.permutations }

                SetPermutation perm ->
                    pure { model | permutations = List.setAt i perm model.permutations }

                SaveEditedPermutation perm ->
                    pure
                        { model
                            | permutations = List.setAt i perm model.permutations
                            , permutationEdit = NotEditing
                        }

        SaveNameEdit ->
            case model.permutationEdit of
                EditingSavedName oldName newName ->
                    pure
                        { model
                            | savedPermutations =
                                case Dict.get oldName model.savedPermutations of
                                    Just perm ->
                                        Dict.insert newName perm (Dict.remove oldName model.savedPermutations)

                                    Nothing ->
                                        model.savedPermutations
                        }

                _ ->
                    pure model

        SetPermutationName newName ->
            pure
                { model
                    | permutationEdit =
                        case model.permutationEdit of
                            EditingSavedName oldName _ ->
                                EditingSavedName oldName newName

                            other ->
                                other
                }


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
view { permutations, savedPermutations, setSize, canvasImage, permutationEdit } =
    let
        permCount =
            List.length permutations

        canvasWidth =
            permCount * canvasImage.horizontalDist + 2 * round canvasImage.paddingX

        canvasHeight =
            setSize * canvasImage.verticalDist + 2 * round canvasImage.paddingY
    in
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "300px auto"
        ]
        [ imageConfigControls canvasImage setSize permutations savedPermutations permutationEdit
        , Canvas.toHtml ( canvasWidth, canvasHeight )
            [ Mouse.onDown (.offsetPos >> StartAt)
            , Mouse.onMove (.offsetPos >> MoveAt)
            , Mouse.onUp (.offsetPos >> EndAt)
            ]
            (Canvas.clear ( 0, 0 ) (toFloat canvasWidth) (toFloat canvasHeight)
                :: permutationLines canvasImage setSize permutations
                :: permutationCircles canvasImage setSize permutations
                :: permutationTexts canvasImage setSize permutations
            )
        ]


imageConfigControls : CanvasImage -> Int -> List Permutation -> Dict String Permutation -> EditState -> Html Msg
imageConfigControls canvasImage n permutations savedPermutations editState =
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
        , Html.h3 [] [ Html.text "Composition" ]
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
                    case Permutation.fixedPoints composition of
                        [] ->
                            "none"

                        fps ->
                            String.join ", " <|
                                List.map String.fromInt fps
                ]
            ]
        , Html.h3 [] [ Html.text "Saved" ]
        , Html.div [] <|
            List.map
                (\( name, p ) -> viewSavedPermutation name p editState)
                (Dict.toList savedPermutations)
        ]


viewPermutation : Int -> Permutation -> EditState -> Int -> Html Msg
viewPermutation index perm editState permCount =
    case editState of
        NotEditing ->
            viewPermutationPlain index permCount perm

        EditingSavedName _ _ ->
            viewPermutationPlain index permCount perm

        EditingPermutation (Saved _) _ _ ->
            viewPermutationPlain index permCount perm

        EditingPermutation (Composed editedIndex) cyclesStr parseRes ->
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
                            [ Html.input (onEnter (UpdateComposed index (SaveEditedPermutation newPerm)) :: inputAttrs) []
                            , cancelButton
                            , Html.button
                                [ HE.onClick (UpdateComposed index (SaveEditedPermutation newPerm)) ]
                                [ Html.text "Save" ]
                            ]

            else
                viewPermutationPlain index permCount perm


viewSavedPermutation : String -> Permutation -> EditState -> Html Msg
viewSavedPermutation name perm editState =
    case editState of
        NotEditing ->
            viewSavedPermutationPlain name Nothing perm

        EditingSavedName oldName editedName ->
            if name == oldName then
                viewSavedPermutationPlain name (Just editedName) perm

            else
                viewSavedPermutationPlain name Nothing perm

        EditingPermutation (Composed _) _ _ ->
            viewSavedPermutationPlain name Nothing perm

        EditingPermutation (Saved editedName) cyclesStr parseRes ->
            if name == editedName then
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
                            [ Html.input (onEnter (UpdateSaved name (SaveEditedPermutation newPerm)) :: inputAttrs) []
                            , cancelButton
                            , Html.button
                                [ HE.onClick (UpdateSaved name (SaveEditedPermutation newPerm)) ]
                                [ Html.text "Save" ]
                            ]

            else
                viewSavedPermutationPlain name Nothing perm


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
    Html.div [ HA.style "border" "dotted black 1px" ]
        [ Html.button [ HE.onClick (UpdateComposed index ResetPermutation), HA.title "Reset to identity" ] [ Html.text "↺" ]
        , Html.button [ HE.onClick (UpdateComposed index GeneratePermutation), HA.title "Generate random permutation" ] [ Html.text "⚄" ]
        , Html.button [ HE.onClick (UpdateComposed index InvertPermutation), HA.title "Invert" ] [ Html.text "🠔" ]
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
        , Html.button [ HE.onClick (UpdateComposed index (StartPermutationEdit perm)), HA.title "Edit permutation" ] [ Html.text "🖉" ]
        , Html.button [ HE.onClick (UpdateComposed index RemovePermutation), HA.title "Delete permutation" ] [ Html.text "✕" ]
        , Html.text <| Permutation.showCycles perm
        ]


viewSavedPermutationPlain : String -> Maybe String -> Permutation -> Html Msg
viewSavedPermutationPlain name maybeEditedName perm =
    Html.div [ HA.style "border" "dotted black 1px" ]
        [ case maybeEditedName of
            Nothing ->
                Html.div []
                    [ Html.text name
                    , Html.button [ HE.onClick (StartNameEdit name), HA.title "Edit name" ] [ Html.text "🖉" ]
                    ]

            Just newName ->
                Html.div []
                    [ Html.input
                        [ onEnter SaveNameEdit
                        , HA.value newName
                        , HE.onInput SetPermutationName
                        , HA.id permutationInputId
                        ]
                        []
                    , Html.button [ HE.onClick CancelEdit ] [ Html.text "Cancel" ]
                    , Html.button
                        [ HE.onClick SaveNameEdit ]
                        [ Html.text "Save" ]
                    ]
        , Html.map (UpdateSaved name) <|
            Html.div []
                [ Html.button [ HE.onClick ResetPermutation, HA.title "Reset to identity" ] [ Html.text "↺" ]
                , Html.button [ HE.onClick GeneratePermutation, HA.title "Generate random permutation" ] [ Html.text "⚄" ]
                , Html.button [ HE.onClick InvertPermutation, HA.title "Invert" ] [ Html.text "🠔" ]
                , Html.button [ HE.onClick (StartPermutationEdit perm), HA.title "Edit permutation" ] [ Html.text "🖉" ]
                , Html.button [ HE.onClick RemovePermutation, HA.title "Delete permutation" ] [ Html.text "✕" ]
                , Html.text <| Permutation.showCycles perm
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TODO add a way to edit permutation without altering composition
-- TODO CSS: align controls and buttons in one column
-- TODO add way to save particular permutation
-- TODO add save button to composed perms
-- TODO add button to add saved to composed
-- TODO add hard-wired model with pre-saved rubik's cube generating permutations
-- TODO add button to reset canvas visual controls
