module Sheet exposing (Direction(..), Position, Sheet, empty, insertFormula, next, raw, render)

import Char
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Parser exposing (..)
import Parser.LanguageKit exposing (tuple)


type Sheet
    = Sheet
        { cells : Dict ( Int, Int ) String
        }


type alias Position =
    { row : Int
    , column : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


next : Direction -> Position -> Position
next direction position =
    case direction of
        Up ->
            { position | row = position.row - 1 }

        Down ->
            { position | row = position.row + 1 }

        Left ->
            { position | column = position.column - 1 }

        Right ->
            { position | column = position.column + 1 }


empty : Sheet
empty =
    Sheet { cells = Dict.empty }


insertFormula : Position -> String -> Sheet -> Sheet
insertFormula { row, column } raw (Sheet state) =
    let
        cells =
            if String.isEmpty raw then
                Dict.remove ( row, column ) state.cells
            else
                Dict.insert ( row, column ) raw state.cells
    in
    Sheet { state | cells = cells }


raw : Position -> Sheet -> String
raw { row, column } (Sheet { cells }) =
    case Dict.get ( row, column ) cells of
        Nothing ->
            ""

        Just formula ->
            formula


render : Position -> Sheet -> Html msg
render { row, column } (Sheet { cells }) =
    case Dict.get ( row, column ) cells of
        Nothing ->
            Char.fromCode {- &nbsp; -} 0xA0
                |> String.fromChar
                |> Html.text

        Just formula ->
            run parser formula
                |> Result.mapError BadParse
                |> Result.andThen (solve cells)
                |> toHtml


toHtml : Result Error String -> Html msg
toHtml result =
    case result of
        Ok string ->
            Html.text string

        Err error ->
            Html.span []
                [ Html.text "#ERROR!"
                , Html.input
                    [ Html.Attributes.type_ "hidden"
                    , Html.Attributes.value <| toString error
                    ]
                    []
                ]



-- PARSE


type Formula
    = Text String
    | Number Float
    | Function String (List Formula)


type Error
    = BadParse Parser.Error
    | BadFunction


parser : Parser Formula
parser =
    oneOf
        [ succeed identity
            |. symbol "="
            |= equation
        , succeed Text
            |= keep zeroOrMore (\_ -> True)
        ]
        |. end


equation : Parser Formula
equation =
    oneOf
        [ succeed Number
            |= float
        , succeed Function
            |= keep oneOrMore isLetter
            |. spaces
            |= tuple spaces (lazy (\_ -> equation))
            |. spaces
        ]


spaces : Parser ()
spaces =
    ignore zeroOrMore ((==) ' ')


isLetter : Char -> Bool
isLetter c =
    Char.isUpper c || Char.isLower c



-- INTERPRET


solve : Dict ( Int, Int ) String -> Formula -> Result Error String
solve cells formula =
    case formula of
        Text value ->
            Ok value

        Number value ->
            Ok <| toString value

        Function name args ->
            solveFunction cells name args
                |> Maybe.map toString
                |> Result.fromMaybe BadFunction


solveFunction : Dict ( Int, Int ) String -> String -> List Formula -> Maybe Float
solveFunction cells name args =
    case String.toUpper name of
        "SUM" ->
            Maybe.map List.sum <| validateAll (solveFloat cells) args

        "MIN" ->
            Maybe.andThen List.minimum <| validateAll (solveFloat cells) args

        "MAX" ->
            Maybe.andThen List.maximum <| validateAll (solveFloat cells) args

        _ ->
            Nothing


solveFloat : Dict ( Int, Int ) String -> Formula -> Maybe Float
solveFloat cells formula =
    case formula of
        Text _ ->
            Nothing

        Number value ->
            Just value

        Function name args ->
            solveFunction cells name args


validateAll : (a -> Maybe b) -> List a -> Maybe (List b)
validateAll func =
    List.foldr (func >> Maybe.map2 (::)) (Just [])
