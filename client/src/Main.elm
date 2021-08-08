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
    , permutationNames : List String
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
    | UpdatePermutation String PermutationUpdate
    | StartNameEdit String
    | SaveNameEdit
    | SetPermutationName String
    | ShiftPermutationLeft Int
    | ShiftPermutationRight Int
    | RemovePermutationFromComposition Int
    | CancelEdit
    | SetPermutationString String
    | SetPermutations (List Permutation)
    | AddPermutationToComposition String
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
      -- TODO refactor so that name and perm can be edited at once
    | EditingPermutation String String (Result String Permutation)
    | EditingSavedName String String


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
    let
        initialPerms =
            [ Permutation.identity n
            , Permutation (Array.fromList [ 1, 2, 0 ])
            , Permutation (Array.fromList [ 2, 0, 1 ])
            ]
                |> List.map (\p -> ( Permutation.showCycles p, p ))
    in
    ( { movingCircle = Nothing
      , circle = ( 100, 100 )
      , permutationNames = List.map Tuple.first initialPerms
      , savedPermutations = Dict.fromList initialPerms
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
                    , permutationNames = List.map (always "id") model.permutationNames
                    , savedPermutations = Dict.fromList [ ( "id", Permutation.identity newN ) ]
                }

        ShiftPermutationLeft i ->
            if i > 0 then
                pure { model | permutationNames = List.swapAt (i - 1) i model.permutationNames }

            else
                pure model

        ShiftPermutationRight i ->
            if i < List.length model.permutationNames then
                pure { model | permutationNames = List.swapAt i (i + 1) model.permutationNames }

            else
                pure model

        GenerateAll ->
            ( model, Cmd.map SetPermutations (Permutation.generateMany model.setSize (List.length model.permutationNames)) )

        SetPermutations ps ->
            -- TODO make this work under the name "named permutation" model
            pure model

        ResetAll ->
            pure
                { model
                    | permutationNames = List.map (always "id") model.permutationNames
                    , savedPermutations = Dict.insert "id" (Permutation.identity model.setSize) model.savedPermutations
                }

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

        StartNameEdit oldName ->
            ( { model | permutationEdit = EditingSavedName oldName "" }
            , focusPermutationInput
            )

        UpdatePermutation permName permutationUpdate ->
            case permutationUpdate of
                ResetPermutation ->
                    pure { model | savedPermutations = Dict.update permName (Maybe.map (always (Permutation.identity model.setSize))) model.savedPermutations }

                GeneratePermutation ->
                    ( model, Cmd.map (UpdatePermutation permName << SetPermutation) (Permutation.generate model.setSize) )

                InvertPermutation ->
                    pure { model | savedPermutations = Dict.update permName (Maybe.map Permutation.inverse) model.savedPermutations }

                StartPermutationEdit perm ->
                    let
                        cycles =
                            Permutation.showCycles perm
                    in
                    ( { model | permutationEdit = EditingPermutation permName cycles (Permutation.parseCycles model.setSize cycles) }
                    , focusPermutationInput
                    )

                RemovePermutation ->
                    pure { model | savedPermutations = Dict.remove permName model.savedPermutations }

                SetPermutation perm ->
                    pure
                        { model
                            | savedPermutations =
                                -- After generating the perm, we remove it under old name
                                -- and insert it under the default new name, which we set to cycles string by default
                                Dict.insert (Permutation.showCycles perm) perm <|
                                    Dict.remove permName model.savedPermutations
                        }

                SaveEditedPermutation perm ->
                    pure
                        { model
                            | savedPermutations = Dict.insert permName perm model.savedPermutations
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

        AddPermutationToComposition name ->
            pure { model | permutationNames = model.permutationNames ++ [ name ] }

        RemovePermutationFromComposition i ->
            pure { model | permutationNames = List.removeAt i model.permutationNames }


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
view { permutationNames, savedPermutations, setSize, canvasImage, permutationEdit } =
    let
        permCount =
            List.length permutationNames

        permutations =
            List.filterMap (\name -> Dict.get name savedPermutations) permutationNames

        canvasWidth =
            permCount * canvasImage.horizontalDist + 2 * round canvasImage.paddingX

        canvasHeight =
            setSize * canvasImage.verticalDist + 2 * round canvasImage.paddingY
    in
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "300px auto"
        ]
        [ imageConfigControls canvasImage setSize permCount permutations savedPermutations permutationEdit
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


imageConfigControls : CanvasImage -> Int -> Int -> List Permutation -> Dict String Permutation -> EditState -> Html Msg
imageConfigControls canvasImage setSize permCount permutations savedPermutations editState =
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
                    , HA.value (String.fromInt setSize)
                    , HE.onInput (Maybe.withDefault NoOp << Maybe.map (SetN << clamp 1 50) << String.toInt)
                    ]
                    []
                ]
            ]
        , Html.h3 [] [ Html.text "Saved" ]
        , Html.div [] <|
            List.map
                (\( name, p ) -> viewSavedPermutation name p editState)
                (Dict.toList savedPermutations)
        , Html.h3 [] [ Html.text "Composition" ]
        , Html.div []
            [ Html.button [ HE.onClick GenerateAll ] [ Html.text "Gen all" ]
            , Html.button [ HE.onClick ResetAll ] [ Html.text "Reset all" ]
            ]
        , Html.div [] <|
            List.indexedMap (viewPermutation permCount) permutations
        , let
            composition =
                List.foldr Permutation.compose (Permutation.identity setSize) permutations
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


viewSavedPermutation : String -> Permutation -> EditState -> Html Msg
viewSavedPermutation name perm editState =
    case editState of
        EditingSavedName oldName newName ->
            if name == oldName then
                viewSavedPermutationHelp name perm (Just newName) Nothing

            else
                viewSavedPermutationHelp name perm Nothing Nothing

        EditingPermutation editedName cyclesStr parseRes ->
            if name == editedName then
                viewSavedPermutationHelp name perm Nothing (Just ( cyclesStr, parseRes ))

            else
                viewSavedPermutationHelp name perm Nothing Nothing

        NotEditing ->
            viewSavedPermutationHelp name perm Nothing Nothing


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


viewPermutation : Int -> Int -> Permutation -> Html Msg
viewPermutation permCount index perm =
    Html.div [ HA.style "border" "dotted black 1px" ]
        [ Html.button
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
        , Html.text <| Permutation.showCycles perm
        , Html.button
            [ HE.onClick (RemovePermutationFromComposition index)
            , HA.title "Delete permutation"
            , HA.style "float" "right"
            ]
            [ Html.text "✕" ]
        ]


viewSavedPermutationHelp : String -> Permutation -> Maybe String -> Maybe ( String, Result String Permutation ) -> Html Msg
viewSavedPermutationHelp name perm maybeEditedName maybeEditedPerm =
    Html.div [ HA.style "border" "dotted black 1px" ]
        [ viewSavedPermutationName name maybeEditedName
        , viewSavedPermutationPermutation name maybeEditedPerm perm
        ]


viewSavedPermutationPermutation : String -> Maybe ( String, Result String Permutation ) -> Permutation -> Html Msg
viewSavedPermutationPermutation name maybeEditedPerm perm =
    case maybeEditedPerm of
        Nothing ->
            Html.div []
                [ Html.button [ HE.onClick (UpdatePermutation name ResetPermutation), HA.title "Reset to identity" ] [ Html.text "↺" ]
                , Html.button [ HE.onClick (UpdatePermutation name GeneratePermutation), HA.title "Generate random permutation" ] [ Html.text "⚄" ]
                , Html.button [ HE.onClick (UpdatePermutation name InvertPermutation), HA.title "Invert" ] [ Html.text "🠔" ]
                , Html.button [ HE.onClick (UpdatePermutation name (StartPermutationEdit perm)), HA.title "Edit permutation" ] [ Html.text "🖉" ]
                , Html.text <| Permutation.showCycles perm
                , Html.button [ HE.onClick (AddPermutationToComposition name) ] [ Html.text "Add" ]
                , Html.button [ HE.onClick (UpdatePermutation name RemovePermutation), HA.title "Delete permutation", HA.style "float" "right" ] [ Html.text "✕" ]
                ]

        Just ( cyclesStr, parseRes ) ->
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
                        [ Html.input (onEnter (UpdatePermutation name (SaveEditedPermutation newPerm)) :: inputAttrs) []
                        , cancelButton
                        , Html.button
                            [ HE.onClick (UpdatePermutation name (SaveEditedPermutation newPerm)) ]
                            [ Html.text "Save" ]
                        ]


viewSavedPermutationName : String -> Maybe String -> Html Msg
viewSavedPermutationName name maybeEditedName =
    Html.div [] <|
        case maybeEditedName of
            Nothing ->
                [ Html.text name
                , Html.button [ HE.onClick (StartNameEdit name), HA.title "Edit Name" ] [ Html.text "🖉" ]
                ]

            Just newName ->
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
-- TODO add buttons to shift left/right ignoring effect on composition
-- TODO should composed perms be always subset of saved ones?
-- TODO after adding saved to composition, show names of saved ones in composition
