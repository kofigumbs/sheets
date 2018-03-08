module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
    { rows : Int
    , columns : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { rows = 20
      , columns = 5
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    List.repeat model.columns viewCell
        |> List.repeat model.rows
        |> List.intersperse [ br [] [] ]
        |> List.concat
        |> div []


viewCell : Html Msg
viewCell =
    button [] []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
