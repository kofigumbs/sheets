module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sheet
import Task
import Ui.Direction exposing (Direction)
import Ui.Grid
import Ui.Palette


-- MODEL


type alias Model =
    { selected : Sheet.Position
    , sheet : Sheet.Sheet
    , dimmensions : Ui.Grid.Dimmensions
    }


init : ( Model, Cmd Msg )
init =
    ( { selected = Sheet.Position 1 1
      , sheet = Sheet.empty
      , dimmensions = { rows = 100, columns = 26 }
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
        Ui.Direction.Up ->
            { position | row = position.row - 1 }

        Ui.Direction.Down ->
            { position | row = position.row + 1 }

        Ui.Direction.Left ->
            { position | column = position.column - 1 }

        Ui.Direction.Right ->
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
                , ( "border-bottom", "solid 1px " ++ Ui.Palette.darkGrey )
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
                , Ui.Direction.onKeyDown SelectNext
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
            [ Ui.Grid.view SelectCell model.dimmensions model.selected model.sheet
            ]
        ]


textValue : Maybe String -> Attribute msg
textValue =
    value << Maybe.withDefault ""


formulaPad : String
formulaPad =
    "35px"



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
