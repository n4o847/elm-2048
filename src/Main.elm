module Main exposing (Board, Cell(..), Model, Msg(..), Position, Row, init, main, setBoard, update, view, viewBoard, viewCell, viewRow)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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


type alias Position =
    ( Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            Array.repeat 4 <| Array.repeat 4 <| Tile 2
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change ->
            ( { model
                | board = setBoard ( 1, 1 ) (Tile 4) model.board
              }
            , Cmd.none
            )


setBoard : Position -> Cell -> Board -> Board
setBoard ( i, j ) cell board =
    Array.get i board
        |> Maybe.map (\oldRow -> Array.set j cell oldRow)
        |> Maybe.map (\newRow -> Array.set i newRow board)
        |> Maybe.withDefault board



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello" ]
        , viewBoard model.board
        , button [ onClick Change ] [ text "Change" ]
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [] <|
        Array.toList (Array.map viewRow board)


viewRow : Row -> Html Msg
viewRow row =
    div [] <|
        Array.toList (Array.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile number ->
            span []
                [ text (String.fromInt number)
                ]

        Empty ->
            span [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
