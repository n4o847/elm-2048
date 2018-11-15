import Browser
import Html exposing (..)
import Html.Events exposing (..)

import Array exposing (Array)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { board : Board
    }

type alias Board =
    Array Row

type alias Row =
    Array Cell

type Cell
    = Tile Int
    | Empty


init : Model
init =
    { board =
        Array.repeat 4 <| Array.repeat 4 <| Tile 2
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello" ]
        , viewBoard model.board
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div []
        <| Array.toList (Array.map viewRow board)


viewRow : Row -> Html Msg
viewRow row =
    div []
        <| Array.toList (Array.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile number ->
            span []
            [ text (String.fromInt number)
            ]

        Empty ->
            span [] []
