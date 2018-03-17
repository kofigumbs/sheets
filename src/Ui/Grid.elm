module Ui.Grid exposing (Dimmensions, view)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Sheet exposing (Sheet)
import Ui.Palette


type alias Dimmensions =
    { rows : Int
    , columns : Int
    }


view : (Sheet.Position -> msg) -> Dimmensions -> Sheet.Position -> Sheet -> Html msg
view toSelect dimmensions selected sheet =
    table
        [ style
            [ ( "width", "100%" )
            , ( "table-layout", "fixed" )
            , ( "border-collapse", "collapse" )
            ]
        ]
    <|
        letterLabels dimmensions.columns selected
            :: axis dimmensions.rows (dataRow toSelect dimmensions selected sheet)


dataRow : (Sheet.Position -> msg) -> Dimmensions -> Sheet.Position -> Sheet -> Int -> Html msg
dataRow toSelect { columns } selected sheet index =
    tr [] <|
        numberLabel index selected
            :: axis columns (dataCell toSelect selected sheet << Sheet.Position index)


dataCell : (Sheet.Position -> msg) -> Sheet.Position -> Sheet -> Sheet.Position -> Html msg
dataCell toSelect selected sheet current =
    cell
        [ dataStyle current selected
        , onClick <| toSelect current
        ]
        [ case Sheet.evaluate sheet current of
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
        [ tr [] <| cornerCell :: axis amount (letterLabelCell position) ]


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
    cell [ labelStyle Ui.Palette.lightGrey ] [ text "" ]


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
            [ ( "border", "double 2px " ++ Ui.Palette.highlightBlue )
            , ( "text-align", "left" )
            ]
    else
        style
            [ ( "border", "solid 1px " ++ Ui.Palette.mediumGray )
            , ( "text-align", "right" )
            ]


labelStyle : String -> Attribute msg
labelStyle color =
    style
        [ ( "text-align", "center" )
        , ( "background-color", color )
        , ( "border", "double 1px " ++ Ui.Palette.darkGrey )
        ]


labelColor : Int -> Int -> String
labelColor current selected =
    if current == selected then
        Ui.Palette.mediumGray
    else
        Ui.Palette.lightGrey


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
