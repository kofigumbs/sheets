module Ui.Grid exposing (Dimmensions, view)

import Char
import Sheet exposing (Sheet)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Svg.Lazy exposing (..)
import Ui.Palette


-- CONFIG


cellWidth : Int
cellWidth =
    110


cellHeight : Int
cellHeight =
    25


cellPadding : String
cellPadding =
    "7"


fontColor : String
fontColor =
    "black"



-- DRAW THE SVG


type alias Dimmensions =
    { rows : Int
    , columns : Int
    }


view : (Sheet.Position -> msg) -> Dimmensions -> Sheet.Position -> Sheet -> Svg msg
view toSelect dimmensions selected sheet =
    svg
        [ width <| toString <| cellWidth * (dimmensions.columns + 1)
        , height <| toString <| cellHeight * (dimmensions.rows + 1)
        , fill "white"
        , fontFamily "sans-serif"
        ]
        [ letterLabels dimmensions.columns selected
        , axis dimmensions.rows <| row toSelect dimmensions selected sheet
        ]


row : (Sheet.Position -> msg) -> Dimmensions -> Sheet.Position -> Sheet -> Int -> Svg msg
row toSelect { columns } selected sheet index =
    g [ scaledTransform index 0 ]
        [ numberLabel selected index
        , axis columns <| dataCell toSelect selected sheet << Sheet.Position index
        ]


dataCell : (Sheet.Position -> msg) -> Sheet.Position -> Sheet -> Sheet.Position -> Svg msg
dataCell toSelect selected sheet current =
    cell
        [ scaledTransform 0 current.column
        , dataStyle current selected
        , onClick <| toSelect current
        ]
        [ case Sheet.evaluate sheet current of
            Sheet.Empty ->
                cellText nbsp

            Sheet.Success value ->
                cellText value

            Sheet.Error reason ->
                g []
                    [ cellText "#ERROR!"
                    , Svg.title [] [ text <| toString reason ]
                    ]
        ]


letterLabels : Int -> Sheet.Position -> Svg msg
letterLabels columns position =
    g [] [ cornerCell, axis columns (letterLabelCell position) ]


letterLabelCell : Sheet.Position -> Int -> Svg msg
letterLabelCell { column } index =
    cell
        [ scaledTransform 0 index
        , labelStyle <| labelColor index column
        ]
        [ cellText <| Sheet.columnName index ]


numberLabel : Sheet.Position -> Int -> Svg msg
numberLabel position index =
    cell
        [ labelStyle <| labelColor index position.row
        ]
        [ cellText <| toString index ]


cornerCell : Svg msg
cornerCell =
    cell [ labelStyle Ui.Palette.lightGrey ] [ cellText nbsp ]


cell : List (Attribute msg) -> List (Svg msg) -> Svg msg
cell extras children =
    g (withCellSize extras) <| rect (withCellSize []) [] :: children


scaledTransform : Int -> Int -> Attribute msg
scaledTransform rowIndex columnIndex =
    transform <|
        "translate("
            ++ toString (columnIndex * cellWidth)
            ++ ","
            ++ toString (rowIndex * cellHeight)
            ++ ")"


dataStyle : Sheet.Position -> Sheet.Position -> Attribute msg
dataStyle current selected =
    Svg.Attributes.style <|
        if current == selected then
            "stroke-width:2px;stroke:" ++ Ui.Palette.highlightBlue
        else
            "stroke-width:1px;stroke:" ++ Ui.Palette.mediumGray


labelStyle : String -> Attribute msg
labelStyle color =
    Svg.Attributes.style
        ("fill:" ++ color ++ ";stroke-width:1px;stroke:" ++ Ui.Palette.darkGrey)


labelColor : Int -> Int -> String
labelColor current selected =
    if current == selected then
        Ui.Palette.mediumGray
    else
        Ui.Palette.lightGrey


withCellSize : List (Attribute msg) -> List (Attribute msg)
withCellSize extras =
    width (toString cellWidth) :: height (toString cellHeight) :: extras


cellText : String -> Svg msg
cellText value =
    text_
        [ Svg.Attributes.style <|
            "stroke:none;fill:"
                ++ fontColor
                ++ ";"
                ++ disableSelect
        , alignmentBaseline "before-edge"
        ]
        [ text value ]


axis : Int -> (Int -> Svg msg) -> Svg msg
axis n =
    g [] << axisHelp [] n


axisHelp : List a -> Int -> (Int -> a) -> List a
axisHelp acc n func =
    if n == 0 then
        acc
    else
        axisHelp (func n :: acc) (n - 1) func


nbsp : String
nbsp =
    String.fromChar <| Char.fromCode 0xA0


disableSelect : String
disableSelect =
    """
    -webkit-user-select:none;
    -moz-user-select:none;
    -ms-user-select:none;
    user-select:none;
    """
