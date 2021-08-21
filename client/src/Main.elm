module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced as CSA
import Canvas.Settings.Line as CS
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color
import Dict exposing (Dict)
import DnDList
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Permutation exposing (Permutation(..))
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
    { permutationIndices : List Int
    , savedPermutations : Dict Int ( String, Permutation )
    , setSize : Int
    , canvasImage : CanvasImage
    , permutationEdit : Maybe EditState
    , highlightedIndex : Maybe Int
    , compositionDndModel : DnDList.Model
    }


type Msg
    = -- Visual controls
      SetHorizontalDist Int
    | SetVerticalDist Int
    | SetCircleRadius Float
    | SetPaddingX Float
    | SetPaddingY Float
    | ResetImage
    | HighlightIndex Int
    | ClearHighlight
      -- Changing composition
    | RemovePermutationFromComposition Int
    | AddPermutationToComposition Int
    | GenerateAll
    | ResetAll
    | DndPermutation DnDList.Msg
      -- Changing saved perms
    | NewSavedPermutation
    | DeleteUnused
    | UpdatePermutation Int PermutationUpdate
    | CancelEdit
    | SaveEditedPermutation Int Permutation
    | SetPermutationName String
    | SetPermutationCycles String
    | SetPermutations (List Permutation)
    | SetSetSize Int
    | NoOp


type PermutationUpdate
    = ResetPermutation
    | GeneratePermutation
    | InvertPermutation
    | StartPermutationEdit
    | RemovePermutation
    | SetPermutation Permutation


type alias CanvasImage =
    { horizontalDist : Int
    , verticalDist : Int
    , circleRadius : Float
    , paddingX : Float
    , paddingY : Float
    }


type alias EditState =
    { editedIndex : Int
    , newName : String
    , cyclesString : String
    , parsedCycles : Result String Permutation
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
    let
        setSize =
            4

        permCount =
            3
    in
    ( { permutationIndices = []
      , savedPermutations = Dict.empty
      , setSize = setSize
      , canvasImage = defaultImage
      , permutationEdit = Nothing
      , highlightedIndex = Nothing
      , compositionDndModel = compositionDndSystem.model
      }
    , Cmd.map SetPermutations (Permutation.generateMany setSize permCount)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pure model

        HighlightIndex i ->
            pure { model | highlightedIndex = Just i }

        ClearHighlight ->
            pure { model | highlightedIndex = Nothing }

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

        ResetImage ->
            pure { model | canvasImage = defaultImage }

        SetSetSize newN ->
            pure
                { model
                    | setSize = newN
                    , permutationIndices = List.map (always 0) model.permutationIndices
                    , savedPermutations = Dict.fromList [ ( 0, ( "id", Permutation.identity newN ) ) ]
                }

        GenerateAll ->
            ( model, Cmd.map SetPermutations (Permutation.generateMany model.setSize (List.length model.permutationIndices)) )

        SetPermutations newPerms ->
            let
                ( newSavedPerms, newPermIndices ) =
                    List.foldr
                        (\newPerm ( savedPerms, permIndices ) ->
                            case
                                -- Before adding new perm to saved ones, check if we can reuse one of the saved ones
                                -- TODO this is quadratic. Can we do better?
                                Dict.toList savedPerms
                                    |> List.find (\( _, ( _, p ) ) -> newPerm == p)
                            of
                                Just ( existingIdx, _ ) ->
                                    ( savedPerms
                                    , existingIdx :: permIndices
                                    )

                                Nothing ->
                                    let
                                        newPermIndex =
                                            Dict.keys savedPerms |> List.maximum |> Maybe.withDefault 0 |> (+) 1

                                        newPermName =
                                            "p" ++ String.fromInt newPermIndex
                                    in
                                    ( Dict.insert newPermIndex ( newPermName, newPerm ) savedPerms
                                    , newPermIndex :: permIndices
                                    )
                        )
                        ( model.savedPermutations, [] )
                        newPerms
            in
            pure
                { model
                    | permutationIndices = newPermIndices
                    , savedPermutations = newSavedPerms
                }

        ResetAll ->
            pure
                { model
                    | permutationIndices = List.map (always 0) model.permutationIndices
                    , savedPermutations = Dict.insert 0 ( "id", Permutation.identity model.setSize ) model.savedPermutations
                }

        SetPermutationCycles newCyclesString ->
            pure
                { model
                    | permutationEdit =
                        Maybe.map
                            (\editState ->
                                { editState
                                    | cyclesString = newCyclesString
                                    , parsedCycles = Permutation.parseCycles model.setSize newCyclesString
                                }
                            )
                            model.permutationEdit
                }

        SetPermutationName newName ->
            pure
                { model
                    | permutationEdit =
                        Maybe.map
                            (\editState -> { editState | newName = newName })
                            model.permutationEdit
                }

        CancelEdit ->
            pure { model | permutationEdit = Nothing }

        UpdatePermutation idx permutationUpdate ->
            case permutationUpdate of
                ResetPermutation ->
                    pure
                        { model
                            | savedPermutations =
                                Dict.update idx
                                    (Maybe.map
                                        (always
                                            (let
                                                p =
                                                    Permutation.identity model.setSize
                                             in
                                             ( Permutation.showCycles p, p )
                                            )
                                        )
                                    )
                                    model.savedPermutations
                        }

                GeneratePermutation ->
                    ( model, Cmd.map (UpdatePermutation idx << SetPermutation) (Permutation.generate model.setSize) )

                InvertPermutation ->
                    pure { model | savedPermutations = Dict.update idx (Maybe.map (Tuple.mapSecond Permutation.inverse)) model.savedPermutations }

                StartPermutationEdit ->
                    ( { model
                        | permutationEdit =
                            Dict.get idx model.savedPermutations
                                |> Maybe.andThen
                                    (\( name, perm ) ->
                                        let
                                            initialCyclesStr =
                                                Permutation.showCycles perm
                                        in
                                        Just
                                            { editedIndex = idx
                                            , newName = name
                                            , cyclesString = initialCyclesStr
                                            , parsedCycles = Permutation.parseCycles model.setSize initialCyclesStr
                                            }
                                    )
                      }
                    , focusPermutationInput
                    )

                RemovePermutation ->
                    pure
                        { model
                            | savedPermutations = Dict.remove idx model.savedPermutations
                            , permutationIndices = List.filter (\i -> i /= idx) model.permutationIndices
                        }

                SetPermutation perm ->
                    pure
                        { model | savedPermutations = Dict.insert idx ( Permutation.showCycles perm, perm ) model.savedPermutations }

        SaveEditedPermutation idx newPerm ->
            case model.permutationEdit of
                Just editState ->
                    pure
                        { model
                            | savedPermutations =
                                Dict.insert idx ( editState.newName, newPerm ) model.savedPermutations
                            , permutationEdit = Nothing
                        }

                Nothing ->
                    pure model

        AddPermutationToComposition idx ->
            pure { model | permutationIndices = model.permutationIndices ++ [ idx ] }

        RemovePermutationFromComposition i ->
            pure { model | permutationIndices = List.removeAt i model.permutationIndices }

        NewSavedPermutation ->
            let
                newPermIndex =
                    Dict.keys model.savedPermutations |> List.maximum |> Maybe.withDefault 0 |> (+) 1

                newPermName =
                    "p" ++ String.fromInt newPermIndex

                newPermCycles =
                    Permutation.showCycles newPerm

                newPerm =
                    Permutation.identity model.setSize
            in
            pure
                { model
                    | permutationEdit =
                        Just
                            { editedIndex = newPermIndex
                            , newName = newPermName
                            , cyclesString = newPermCycles
                            , parsedCycles = Ok newPerm
                            }
                    , savedPermutations = Dict.insert newPermIndex ( newPermName, newPerm ) model.savedPermutations
                }

        DeleteUnused ->
            pure
                { model
                    | savedPermutations =
                        Dict.filter (\idx _ -> List.member idx model.permutationIndices)
                            model.savedPermutations
                }

        DndPermutation dndMsg ->
            let
                ( newCompositionDndModel, newPermutationIndices ) =
                    compositionDndSystem.update dndMsg model.compositionDndModel model.permutationIndices
            in
            ( { model
                | compositionDndModel = newCompositionDndModel
                , permutationIndices = newPermutationIndices

                --  whenever DnD is in progress, clear all highlights
                , highlightedIndex = Nothing
              }
            , compositionDndSystem.commands newCompositionDndModel
            )


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


updateImage : (CanvasImage -> CanvasImage) -> Model -> Model
updateImage f model =
    { model | canvasImage = f model.canvasImage }


view : Model -> Html Msg
view ({ permutationIndices, savedPermutations, setSize, canvasImage } as model) =
    let
        permCount =
            List.length permutationIndices

        permsWithIndicesAndNames : List ( Int, ( String, Permutation ) )
        permsWithIndicesAndNames =
            List.filterMap
                (\savedIndex ->
                    Maybe.map (\np -> ( savedIndex, np )) <|
                        Dict.get savedIndex savedPermutations
                )
                permutationIndices

        permsWithIndices : List ( Int, Permutation )
        permsWithIndices =
            List.map (Tuple.mapSecond Tuple.second) permsWithIndicesAndNames

        permutations =
            List.map (Tuple.second >> Tuple.second) permsWithIndicesAndNames

        canvasWidth =
            permCount * canvasImage.horizontalDist + 2 * round canvasImage.paddingX

        canvasHeight =
            setSize * canvasImage.verticalDist + 2 * round canvasImage.paddingY
    in
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "300px auto"
        ]
        [ controlsPanel model permutations permsWithIndicesAndNames
        , Canvas.toHtml ( canvasWidth, canvasHeight ) [] <|
            List.concat
                [ [ Canvas.clear ( 0, 0 ) (toFloat canvasWidth) (toFloat canvasHeight) ]
                , permutationLines canvasImage setSize permsWithIndices model.highlightedIndex
                , [ permutationCircles canvasImage setSize permCount ]
                , permutationTexts canvasImage setSize permCount
                ]
        ]


controlsPanel : Model -> List Permutation -> List ( Int, ( String, Permutation ) ) -> Html Msg
controlsPanel ({ savedPermutations, setSize, canvasImage, permutationIndices, highlightedIndex, compositionDndModel } as model) perms permsWithIndicesAndNames =
    Html.div []
        [ imageControlsView canvasImage
        , savedPermutationsView model
        , Html.h3 [] [ Html.text "Composition" ]
        , Html.div []
            [ Html.button [ HE.onClick GenerateAll ] [ Html.text "Gen all" ]
            , Html.button [ HE.onClick ResetAll ] [ Html.text "Reset all" ]
            ]
        , Html.div [] <|
            List.indexedMap (\i -> permutationDndWrapper compositionDndModel i << viewPermutation highlightedIndex i)
                permsWithIndicesAndNames
        , dndGhostView compositionDndModel permutationIndices savedPermutations
        , let
            composition =
                List.foldr Permutation.compose (Permutation.identity setSize) perms
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
        ]


imageControlsView : CanvasImage -> Html Msg
imageControlsView canvasImage =
    Html.div []
        [ Html.h3 [] [ Html.text "Image controls" ]
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
        , Html.button [ HE.onClick ResetImage ] [ Html.text "Reset" ]
        ]


savedPermutationsView : Model -> Html Msg
savedPermutationsView { setSize, savedPermutations, highlightedIndex, permutationEdit } =
    Html.div []
        [ Html.h3 [] [ Html.text "Permutations" ]
        , Html.div []
            [ Html.label []
                [ Html.text "Set size "
                , Html.input
                    [ HA.type_ "number"
                    , HA.min "1"
                    , HA.max "50"
                    , HA.value (String.fromInt setSize)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map (SetSetSize << clamp 1 50) << String.toInt)
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.button [ HE.onClick NewSavedPermutation ] [ Html.text "Add permutation" ]
            , Html.button [ HE.onClick DeleteUnused, HA.title "Remove all saved permutations that are not used in the composition" ]
                [ Html.text "Remove unused" ]
            ]
        , Html.div [] <|
            List.map
                (\( savedIdx, ( name, p ) ) -> viewSavedPermutation highlightedIndex savedIdx name p permutationEdit)
                (Dict.toList savedPermutations)
        ]


viewSavedPermutation : Maybe Int -> Int -> String -> Permutation -> Maybe EditState -> Html Msg
viewSavedPermutation highlightedIndex savedIndex name perm maybeEditState =
    permutationWrapper highlightedIndex savedIndex <|
        case Maybe.filter (\editState -> editState.editedIndex == savedIndex) maybeEditState of
            Just editState ->
                viewPermutationEditor savedIndex editState

            Nothing ->
                viewPermutationPlain savedIndex name perm


permutationWrapper : Maybe Int -> Int -> List (Html Msg) -> Html Msg
permutationWrapper highlightedIndex savedIndex =
    Html.div
        [ HA.style "border" "dotted black 1px"
        , HA.style "background-color" <|
            if highlightedIndex == Just savedIndex then
                "lightcyan"

            else
                "white"
        , HE.onMouseEnter (HighlightIndex savedIndex)
        , HE.onMouseLeave ClearHighlight
        ]


permutationDndWrapper : DnDList.Model -> Int -> Html Msg -> Html Msg
permutationDndWrapper compositionDndModel indexWithinList permView =
    let
        itemId : String
        itemId =
            "id-" ++ String.fromInt indexWithinList
    in
    case compositionDndSystem.info compositionDndModel of
        Just { dragIndex } ->
            if dragIndex /= indexWithinList then
                Html.div (HA.id itemId :: compositionDndSystem.dropEvents indexWithinList itemId)
                    [ permView ]

            else
                Html.div
                    [ HA.id itemId
                    , HA.style "background-color" "lightgray"
                    , HA.style "height" "50px"
                    ]
                    []

        Nothing ->
            Html.div (HA.id itemId :: compositionDndSystem.dragEvents indexWithinList itemId)
                [ permView ]


dndGhostView : DnDList.Model -> List Int -> Dict Int ( String, Permutation ) -> Html.Html Msg
dndGhostView compositionDndModel items savedPermutations =
    let
        maybeDragItem =
            compositionDndSystem.info compositionDndModel
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        List.getAt dragIndex items |> Maybe.map (Tuple.pair dragIndex)
                     --                 items |> List.drop dragIndex |> List.head
                    )
    in
    case maybeDragItem of
        Just ( indexWithinComposition, savedIndex ) ->
            Html.div
                (compositionDndSystem.ghostStyles compositionDndModel)
                [ case Dict.get savedIndex savedPermutations of
                    Just p ->
                        viewPermutation Nothing indexWithinComposition ( savedIndex, p )

                    Nothing ->
                        Html.text ""
                ]

        Nothing ->
            Html.text ""


cyclesInputId : String
cyclesInputId =
    "perm-input"


focusPermutationInput : Cmd Msg
focusPermutationInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus cyclesInputId)


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


viewPermutation : Maybe Int -> Int -> ( Int, ( String, Permutation ) ) -> Html Msg
viewPermutation highlightedIndex indexWithinComposition ( savedIndex, ( name, perm ) ) =
    permutationWrapper highlightedIndex
        savedIndex
        [ Html.div [] [ Html.text name ]
        , Html.text <| Permutation.showCycles perm
        , Html.button
            [ HE.onClick (RemovePermutationFromComposition indexWithinComposition)

            -- Avoid triggering DND when clicking on delete button
            , HE.stopPropagationOn "mousedown" (Decode.succeed ( NoOp, True ))
            , HA.title "Delete permutation"
            , HA.style "float" "right"
            ]
            [ Html.text "✕" ]
        ]


viewPermutationPlain : Int -> String -> Permutation -> List (Html Msg)
viewPermutationPlain idx name perm =
    [ Html.text name
    , Html.div []
        [ Html.button [ HE.onClick (UpdatePermutation idx ResetPermutation), HA.title "Reset to identity" ] [ Html.text "↺" ]
        , Html.button [ HE.onClick (UpdatePermutation idx GeneratePermutation), HA.title "Generate random permutation" ] [ Html.text "⚄" ]
        , Html.button [ HE.onClick (UpdatePermutation idx InvertPermutation), HA.title "Invert" ] [ Html.text "🠔" ]
        , Html.button [ HE.onClick (UpdatePermutation idx StartPermutationEdit), HA.title "Edit permutation" ] [ Html.text "🖉" ]
        , Html.button [ HE.onClick (AddPermutationToComposition idx) ] [ Html.text "Add" ]
        , Html.text <| Permutation.showCycles perm
        , Html.button [ HE.onClick (UpdatePermutation idx RemovePermutation), HA.title "Delete permutation", HA.style "float" "right" ] [ Html.text "✕" ]
        ]
    ]


viewPermutationEditor : Int -> EditState -> List (Html Msg)
viewPermutationEditor idx editState =
    let
        ( saveButtonOrError, addOnEnter ) =
            case editState.parsedCycles of
                Err err ->
                    ( Html.div [ HA.style "color" "red" ] [ Html.text err ]
                    , Basics.identity
                    )

                Ok newPerm ->
                    ( Html.button [ HE.onClick (SaveEditedPermutation idx newPerm) ] [ Html.text "Save" ]
                    , (::) (onEnter (SaveEditedPermutation idx newPerm))
                    )
    in
    [ Html.input
        (addOnEnter
            [ HA.value editState.newName
            , HA.placeholder "Name"
            , HE.onInput SetPermutationName
            ]
        )
        []
    , Html.div []
        [ Html.input
            (addOnEnter
                [ HA.value editState.cyclesString
                , HA.placeholder "Cycles"
                , HE.onInput SetPermutationCycles
                , HA.id cyclesInputId
                ]
            )
            []
        , Html.button [ HE.onClick CancelEdit ] [ Html.text "Cancel" ]
        , saveButtonOrError
        ]
    ]


permutationLines : CanvasImage -> Int -> List ( Int, Permutation ) -> Maybe Int -> List Renderable
permutationLines ci setSize permsWithIndices highlightedIndex =
    let
        linesForOnePerm indexInComposition ( savedIdx, Permutation perm ) =
            let
                ( strokeColor, lineWidthPx ) =
                    if highlightedIndex == Just savedIdx then
                        ( Color.lightRed, 2 )

                    else
                        ( Color.black, 1 )
            in
            Canvas.shapes
                [ fill Color.white
                , stroke strokeColor
                , CS.lineWidth lineWidthPx
                , CSA.transform [ CSA.translate ci.paddingX ci.paddingY ]
                ]
                (List.map
                    (\i ->
                        Canvas.path
                            ( toFloat (indexInComposition * ci.horizontalDist)
                            , toFloat (i * ci.verticalDist)
                            )
                            [ Canvas.lineTo
                                ( toFloat ((indexInComposition + 1) * ci.horizontalDist)
                                , toFloat ((Array.get i perm |> Maybe.withDefault 0) * ci.verticalDist)
                                )
                            ]
                    )
                 <|
                    List.range 0 (setSize - 1)
                )
    in
    List.indexedMap linesForOnePerm permsWithIndices


permutationCircles : CanvasImage -> Int -> Int -> Renderable
permutationCircles ci setSize permCount =
    Canvas.shapes
        [ fill Color.white, stroke Color.black ]
        (List.range 0 permCount
            |> List.concatMap
                (\colIdx ->
                    List.range 0 (setSize - 1)
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


permutationTexts : CanvasImage -> Int -> Int -> List Renderable
permutationTexts ci setSize permCount =
    List.range 0 permCount
        |> List.concatMap
            (\col ->
                List.range 0 (setSize - 1)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    compositionDndSystem.subscriptions model.compositionDndModel


{-| Config for reordering permutation indices within the composition list.
-}
compositionDndConfig : DnDList.Config Int
compositionDndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


compositionDndSystem : DnDList.System Int Msg
compositionDndSystem =
    DnDList.create compositionDndConfig DndPermutation



-- TODO add a way to edit permutation without altering composition
-- TODO CSS: align controls and buttons in one column
-- TODO add hard-wired model with pre-saved rubik's cube generating permutations
-- TODO add button to shift perm right/left without affecting composition
-- TODO when renaming, warn if newName already exists
-- TODO show additional detail about saved perms and composed perm (on hover?): order
-- TODO allow removing duplicate saved perms
