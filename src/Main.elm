module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode


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


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            Array.repeat 4 <| Array.repeat 4 <| Tile 2
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change direction ->
            let
                position =
                    case direction of
                        Left ->
                            ( 1, 0 )

                        Right ->
                            ( 1, 2 )

                        Up ->
                            ( 0, 1 )

                        Down ->
                            ( 2, 1 )

                        Other ->
                            ( 1, 1 )
            in
            ( { model
                | board = setBoard position (Tile 4) model.board
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
    Sub.batch
        [ onKeyDown (Decode.map Change keyDecoder)
        ]


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other
