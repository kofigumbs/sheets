module Sheet exposing (Position, Return(..), Sheet, columnName, empty, evaluate, insert, lookup)

import Char
import Dict exposing (Dict)
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


empty : Sheet
empty =
    Sheet { cells = Dict.empty }


insert : Position -> String -> Sheet -> Sheet
insert { row, column } raw (Sheet state) =
    let
        cells =
            if String.isEmpty raw then
                Dict.remove ( row, column ) state.cells
            else
                Dict.insert ( row, column ) raw state.cells
    in
    Sheet { state | cells = cells }


lookup : Position -> Sheet -> Maybe String
lookup { row, column } (Sheet { cells }) =
    Dict.get ( row, column ) cells



-- TODO -- What happens after the 'Z' column?


columnName : Int -> String
columnName n =
    String.fromChar <| Char.fromCode <| n + 64


columnNumber : String -> Maybe Int
columnNumber s =
    case String.toList s of
        [ c ] ->
            Just <| Char.toCode c - 64

        _ ->
            Nothing



-- EVALUATE CELLS


type Return a
    = Empty
    | Error Reason
    | Success a


evaluate : Sheet -> Position -> Return String
evaluate sheet position =
    case lookup position sheet of
        Nothing ->
            Empty

        Just formula ->
            parse formula
                |> Result.andThen (solve sheet)
                |> either Error Success


either : (x -> b) -> (a -> b) -> Result x a -> b
either bad good result =
    case result of
        Err x ->
            bad x

        Ok x ->
            good x



-- PARSE


type Formula
    = Text String
    | Number Float
    | Function String (List Formula)
    | Reference Position


type Reason
    = BadParse Parser.Error
    | BadFunction
    | BadReference Position


parse : String -> Result Reason Formula
parse =
    run parser >> Result.mapError BadParse


parser : Parser Formula
parser =
    oneOf
        [ succeed identity
            |. symbol "="
            |= equation

        -- TODO leading spaces
        -- TODO trailing spaces
        -- TODO number with trailing non-numeric characters
        , succeed textOrNumber
            |= keep oneOrMore (\_ -> True)
        ]
        |. end


textOrNumber : String -> Formula
textOrNumber raw =
    case String.toFloat raw of
        Err _ ->
            Text raw

        Ok value ->
            Number value


equation : Parser Formula
equation =
    oneOf
        [ succeed Number
            |= float
        , delayedCommitMap Function
            (keep oneOrMore isLetter)
            (tuple spaces <| lazy <| \_ -> equation)
            |. spaces
        , succeed Reference
            |= position
        ]


position : Parser Position
position =
    inContext "reference" <|
        map2 (\column row -> Position row column) letter int


letter : Parser Int
letter =
    keep (Exactly 1) Char.isUpper
        |> andThen
            (columnNumber
                >> Maybe.map succeed
                >> Maybe.withDefault (fail "")
            )


spaces : Parser ()
spaces =
    ignore zeroOrMore ((==) ' ')


isLetter : Char -> Bool
isLetter c =
    Char.isUpper c || Char.isLower c



-- SOLVE


solve : Sheet -> Formula -> Result Reason String
solve sheet formula =
    case formula of
        Text value ->
            Ok value

        Number value ->
            Ok <| toString value

        Function name args ->
            solveFunction sheet name args
                |> Maybe.map toString
                |> Result.fromMaybe BadFunction

        Reference position ->
            lookup position sheet
                |> Result.fromMaybe (BadReference position)
                |> Result.andThen parse
                |> Result.andThen (solve sheet)


solveFunction : Sheet -> String -> List Formula -> Maybe Float
solveFunction sheet name args =
    case String.toUpper name of
        "SUM" ->
            Maybe.map List.sum <| validateAll (solveFloat sheet) args

        "MIN" ->
            Maybe.andThen List.minimum <| validateAll (solveFloat sheet) args

        "MAX" ->
            Maybe.andThen List.maximum <| validateAll (solveFloat sheet) args

        _ ->
            Nothing


solveFloat : Sheet -> Formula -> Maybe Float
solveFloat sheet formula =
    case formula of
        Text _ ->
            Nothing

        Number value ->
            Just value

        Function name args ->
            solveFunction sheet name args

        Reference position ->
            lookup position sheet
                |> Maybe.andThen (parse >> Result.toMaybe)
                |> Maybe.andThen (solveFloat sheet)


validateAll : (a -> Maybe b) -> List a -> Maybe (List b)
validateAll func =
    List.foldr (func >> Maybe.map2 (::)) (Just [])
