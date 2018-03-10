module Direction exposing (Direction(..), onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


onKeyDown : (Direction -> msg) -> Attribute msg
onKeyDown transform =
    onWithOptions "keydown"
        { stopPropagation = True, preventDefault = True }
        (field "keyCode" int
            |> andThen toDirection
            |> map transform
        )


toDirection : Int -> Decoder Direction
toDirection code =
    if code == 9 {- TAB -} then
        checkShift { yes = Left, no = Right }
    else if code == 13 {- ENTER -} then
        checkShift { yes = Up, no = Down }
    else if code == 38 {- UP ARROW -} then
        succeed Up
    else if code == 40 {- DOWN ARROW -} then
        succeed Down
    else
        fail ""


checkShift : { yes : a, no : a } -> Decoder a
checkShift { yes, no } =
    let
        check holding =
            if holding then
                yes
            else
                no
    in
    field "shiftKey" bool
        |> map check
