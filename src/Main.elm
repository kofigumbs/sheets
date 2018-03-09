module Main exposing (..)

import Char
import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (fail, succeed)
import Sheet
import Task


-- MODEL


type alias Model =
    { rows : Int
    , columns : Int
    , selected : Sheet.Position
    , sheet : Sheet.Cells
    , shift : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { rows = 100
      , columns = 26
      , selected = Sheet.Position 1 1
      , sheet = Sheet.empty
      , shift = False
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
    | SetShift Bool
    | SelectCell Sheet.Position
    | SelectNext Sheet.Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputFormula input ->
            ( { model | sheet = Sheet.insertFormula model.selected input model.sheet }, Cmd.none )

        SetShift value ->
            ( { model | shift = value }, Cmd.none )

        SelectCell position ->
            ( { model | selected = position }, focusForumulaInput )

        SelectNext direction ->
            ( { model | selected = Sheet.next direction model.selected }, Cmd.none )



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
                , onKeyUp
                , onKeyDown model.shift
                , onInput InputFormula
                , value <| Sheet.raw model.selected model.sheet
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
    axis (letterLabels model.columns model.selected) model.rows <|
        \row ->
            tr [] <|
                axis (numberLabel row model.selected) model.columns <|
                    \column ->
                        let
                            current =
                                Sheet.Position row column
                        in
                        cell
                            [ dataStyle current model.selected
                            , onClick <| SelectCell current
                            ]
                            [ Sheet.render current model.sheet ]


letterLabels : Int -> Sheet.Position -> Html msg
letterLabels amount (Sheet.Position _ column) =
    tr [] <|
        axis cornerCell amount <|
            \index ->
                cell
                    [ labelStyle <| labelColor index column ]
                    [ text <| String.fromChar <| Char.fromCode <| index + 64 ]


numberLabel : Int -> Sheet.Position -> Html msg
numberLabel index (Sheet.Position row _) =
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


dataStyle : Sheet.Position -> Sheet.Position -> Attribute msg
dataStyle current selected =
    if current == selected then
        style
            [ ( "border", "double 1px " ++ highlightBlue )
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


axis : a -> Int -> (Int -> a) -> List a
axis head n func =
    head :: axisHelp [] n func


axisHelp : List a -> Int -> (Int -> a) -> List a
axisHelp acc n func =
    if n == 0 then
        acc
    else
        axisHelp (func n :: acc) (n - 1) func



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



-- KEYBOARD NAVIGATION AND MODIFIERS


onKeyDown : Bool -> Attribute Msg
onKeyDown shift =
    onWithoutDefaults "keydown" <|
        \code ->
            if code == 16 {- SHIFT -} then
                succeed <| SetShift True
            else if code == 9 {- TAB -} then
                succeed <| SelectNext <| negateIf shift Sheet.Right
            else if code == 13 {- ENTER -} then
                succeed <| SelectNext <| negateIf shift Sheet.Down
            else if code == 38 {- UP ARROW -} then
                succeed <| SelectNext Sheet.Up
            else if code == 40 {- DOWN ARROW -} then
                succeed <| SelectNext Sheet.Down
            else
                fail ""


onKeyUp : Attribute Msg
onKeyUp =
    onWithoutDefaults "keyup" <|
        \code ->
            if code == 16 {- SHIFT -} then
                succeed <| SetShift False
            else
                fail ""


onWithoutDefaults : String -> (Int -> Json.Decode.Decoder msg) -> Attribute msg
onWithoutDefaults direction func =
    onWithOptions direction
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.andThen func <| Json.Decode.field "keyCode" Json.Decode.int)


negateIf : Bool -> Sheet.Direction -> Sheet.Direction
negateIf condition direction =
    if not condition then
        direction
    else
        case direction of
            Sheet.Up ->
                Sheet.Down

            Sheet.Down ->
                Sheet.Up

            Sheet.Left ->
                Sheet.Right

            Sheet.Right ->
                Sheet.Left



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
