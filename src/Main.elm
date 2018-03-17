module Main exposing (..)

import Char
import Direction exposing (Direction)
import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sheet
import Task


-- MODEL


type alias Model =
    { rows : Int
    , columns : Int
    , selected : Sheet.Position
    , sheet : Sheet.Sheet
    }


init : ( Model, Cmd Msg )
init =
    ( { rows = 100
      , columns = 26
      , selected = Sheet.Position 1 1
      , sheet = Sheet.empty
      }
    , focusForumulaInput
    )


formulaInputId : String
formulaInputId =
    "formula-input"


focusForumulaInput : Cmd Msg
focusForumulaInput =
    Dom.focus formulaInputId
        |> Task.attempt (\_ -> NoOp)



-- UPDATE


type Msg
    = NoOp
    | InputFormula String
    | SelectCell Sheet.Position
    | SelectNext Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputFormula input ->
            ( { model | sheet = Sheet.insert model.selected input model.sheet }, Cmd.none )

        SelectCell position ->
            ( { model | selected = position }, focusForumulaInput )

        SelectNext direction ->
            ( { model | selected = move direction model.selected }, Cmd.none )


move : Direction -> Sheet.Position -> Sheet.Position
move direction position =
    case direction of
        Direction.Up ->
            { position | row = position.row - 1 }

        Direction.Down ->
            { position | row = position.row + 1 }

        Direction.Left ->
            { position | column = position.column - 1 }

        Direction.Right ->
            { position | column = position.column + 1 }



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "height", "100vh" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "padding", formulaPad )
                , ( "width", "calc(100vw - (2 * " ++ formulaPad ++ "))" )
                , ( "border-bottom", "solid 1px " ++ darkGrey )
                ]
            ]
            [ span
                [ style
                    [ ( "font-family", "cursive" )
                    , ( "padding-right", "15px" )
                    , ( "user-select", "none" )
                    ]
                ]
                [ text "fx" ]
            , input
                [ type_ "text"
                , id formulaInputId
                , style
                    [ ( "flex", "1" )
                    , ( "font-family", "monospace" )
                    , ( "outline", "none" )
                    ]
                , onInput InputFormula
                , Direction.onKeyDown SelectNext
                , textValue <| Sheet.lookup model.selected model.sheet
                ]
                []
            ]
        , div
            [ style
                [ ( "flex", "1" )
                , ( "overflow", "scroll" )
                ]
            ]
            [ table
                [ style
                    [ ( "width", "100%" )
                    , ( "table-layout", "fixed" )
                    , ( "border-collapse", "collapse" )
                    ]
                ]
                (grid model)
            ]
        ]


grid : Model -> List (Html Msg)
grid model =
    letterLabels model.columns model.selected
        :: axis model.rows (dataRow model)


dataRow : Model -> Int -> Html Msg
dataRow model index =
    tr [] <|
        numberLabel index model.selected
            :: axis model.columns (dataCell model << Sheet.Position index)


dataCell : Model -> Sheet.Position -> Html Msg
dataCell model current =
    cell
        [ dataStyle current model.selected
        , onClick <| SelectCell current
        ]
        [ case Sheet.evaluate model.sheet current of
            Sheet.Empty ->
                text nbsp

            Sheet.Success value ->
                text value

            Sheet.Error reason ->
                span [ title <| toString reason ] [ text "#ERROR!" ]
        ]


letterLabels : Int -> Sheet.Position -> Html msg
letterLabels amount position =
    thead []
        [ tr [] <|
            cornerCell
                :: axis amount (letterLabelCell position)
        ]


letterLabelCell : Sheet.Position -> Int -> Html msg
letterLabelCell { column } index =
    cell
        [ labelStyle <| labelColor index column ]
        [ text <| Sheet.columnName index ]


numberLabel : Int -> Sheet.Position -> Html msg
numberLabel index { row } =
    cell
        [ labelStyle <| labelColor index row ]
        [ text <| toString index ]


cornerCell : Html msg
cornerCell =
    cell [ labelStyle lightGrey ] [ text "" ]


cell : List (Attribute msg) -> List (Html msg) -> Html msg
cell attributes =
    td <|
        style
            [ ( "width", "100px" )
            , ( "max-width", "100px" )
            , ( "padding", "5px" )
            , ( "overflow", "hidden" )
            , ( "white-space", "nowrap" )
            ]
            :: attributes


textValue : Maybe String -> Attribute msg
textValue =
    value << Maybe.withDefault ""


dataStyle : Sheet.Position -> Sheet.Position -> Attribute msg
dataStyle current selected =
    if current == selected then
        style
            [ ( "border", "double 2px " ++ highlightBlue )
            , ( "text-align", "left" )
            ]
    else
        style
            [ ( "border", "solid 1px " ++ mediumGray )
            , ( "text-align", "right" )
            ]


labelStyle : String -> Attribute msg
labelStyle color =
    style
        [ ( "text-align", "center" )
        , ( "background-color", color )
        , ( "border", "double 1px " ++ darkGrey )
        ]


labelColor : Int -> Int -> String
labelColor current selected =
    if current == selected then
        mediumGray
    else
        lightGrey


formulaPad : String
formulaPad =
    "35px"


axis : Int -> (Int -> a) -> List a
axis =
    axisHelp []


axisHelp : List a -> Int -> (Int -> a) -> List a
axisHelp acc n func =
    if n == 0 then
        acc
    else
        axisHelp (func n :: acc) (n - 1) func


nbsp : String
nbsp =
    String.fromChar <| Char.fromCode 0xA0



-- COLOR PALETTE


darkGrey : String
darkGrey =
    "#CCCCCC"


mediumGray : String
mediumGray =
    "#DDDDDD"


lightGrey : String
lightGrey =
    "#F3F3F3"


highlightBlue : String
highlightBlue =
    "#4184F5"



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
