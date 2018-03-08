module Sheet exposing (Cells, Position(..), empty, insertFormula, raw, render)

import Char
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Parser exposing (..)
import Parser.LanguageKit exposing (tuple)


type Cells
    = Cells (Dict ( Int, Int ) String)


type Position
    = Position Int Int


empty : Cells
empty =
    Cells Dict.empty


insertFormula : Position -> String -> Cells -> Cells
insertFormula (Position row column) raw (Cells dict) =
    Cells <|
        if String.isEmpty raw then
            Dict.remove ( row, column ) dict
        else
            Dict.insert ( row, column ) raw dict


raw : Position -> Cells -> String
raw (Position row column) (Cells dict) =
    case Dict.get ( row, column ) dict of
        Nothing ->
            ""

        Just formula ->
            formula


render : Position -> Cells -> Html msg
render (Position row column) (Cells dict) =
    case Dict.get ( row, column ) dict of
        Nothing ->
            Html.text ""

        Just formula ->
            run parser formula
                |> Result.mapError BadParse
                |> Result.andThen (solve dict)
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
solve dict formula =
    case formula of
        Text text ->
            Ok text

        Number number ->
            Ok <| toString number

        Function name args ->
            solveFunction dict name args
                |> Maybe.map toString
                |> Result.fromMaybe BadFunction


solveFunction : Dict ( Int, Int ) String -> String -> List Formula -> Maybe Float
solveFunction dict name args =
    case String.toUpper name of
        "SUM" ->
            Maybe.map List.sum <| validateAll (solveFloat dict) args

        "MIN" ->
            Maybe.andThen List.minimum <| validateAll (solveFloat dict) args

        "MAX" ->
            Maybe.andThen List.maximum <| validateAll (solveFloat dict) args

        _ ->
            Nothing


solveFloat : Dict ( Int, Int ) String -> Formula -> Maybe Float
solveFloat dict formula =
    case formula of
        Text text ->
            Nothing

        Number number ->
            Just number

        Function name args ->
            solveFunction dict name args


validateAll : (a -> Maybe b) -> List a -> Maybe (List b)
validateAll func =
    List.foldr (func >> Maybe.map2 (::)) (Just [])
