module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random


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
    = Slide Direction
    | Put ( Position, Cell )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide direction ->
            let
                slidedBoard =
                    slideBoard direction model.board

                cmd =
                    if model.board == slidedBoard then
                        Cmd.none

                    else
                        randomPosition slidedBoard
                            |> Maybe.map (\position -> Random.pair position randomTile)
                            |> Maybe.map (Random.generate Put)
                            |> Maybe.withDefault Cmd.none
            in
            ( { model
                | board = slidedBoard
              }
            , cmd
            )

        Put ( position, cell ) ->
            ( { model
                | board = setBoard position cell model.board
              }
            , Cmd.none
            )


setBoard : Position -> Cell -> Board -> Board
setBoard ( i, j ) cell board =
    Array.get i board
        |> Maybe.map (\oldRow -> Array.set j cell oldRow)
        |> Maybe.map (\newRow -> Array.set i newRow board)
        |> Maybe.withDefault board


mergeCell : Cell -> Cell -> Cell
mergeCell cellX cellY =
    case cellX of
        Tile x ->
            case cellY of
                Tile y ->
                    Tile (x + y)

                Empty ->
                    Empty

        Empty ->
            Empty


type Accumulator
    = Waiting Cell (List Cell)
    | Done (List Cell)


accumulate : Cell -> Accumulator -> Accumulator
accumulate cell acc =
    if cell == Empty then
        acc

    else
        case acc of
            Waiting waiting done ->
                if waiting == cell then
                    Done (mergeCell cell waiting :: done)

                else
                    Waiting cell (waiting :: done)

            Done done ->
                Waiting cell done


slideRow : List Cell -> List Cell
slideRow row =
    let
        acc =
            List.foldr accumulate (Done []) row

        newRow =
            case acc of
                Waiting waiting done ->
                    waiting :: done

                Done done ->
                    done
    in
    List.repeat (List.length row - List.length newRow) Empty ++ newRow


toListBoard : Board -> List (List Cell)
toListBoard board =
    Array.toList <| Array.map Array.toList board


fromListBoard : List (List Cell) -> Board
fromListBoard board =
    Array.fromList <| List.map Array.fromList board


transpose : List (List Cell) -> List (List Cell)
transpose matrix =
    List.foldr (List.map2 (::)) (List.repeat (List.length matrix) []) matrix


slideBoard : Direction -> Board -> Board
slideBoard direction board =
    board
        |> toListBoard
        |> slideListBoard direction
        |> fromListBoard


slideListBoard : Direction -> List (List Cell) -> List (List Cell)
slideListBoard direction board =
    case direction of
        Left ->
            board
                |> List.map List.reverse
                |> slideListBoard Right
                |> List.map List.reverse

        Right ->
            board
                |> List.map slideRow

        Up ->
            board
                |> transpose
                |> slideListBoard Left
                |> transpose

        Down ->
            board
                |> transpose
                |> slideListBoard Right
                |> transpose

        Other ->
            board


emptyPositionList : Board -> List Position
emptyPositionList board =
    board
        |> Array.map Array.toList
        |> Array.toList
        |> List.indexedMap (\i -> List.indexedMap (\j -> Tuple.pair ( i, j )))
        |> List.concat
        |> List.filterMap
            (\( position, cell ) ->
                case cell of
                    Tile n ->
                        Nothing

                    Empty ->
                        Just position
            )


randomPosition : Board -> Maybe (Random.Generator Position)
randomPosition board =
    let
        positionList =
            emptyPositionList board
    in
    case positionList of
        [] ->
            Nothing

        head :: tail ->
            Just (Random.uniform head tail)


randomTile : Random.Generator Cell
randomTile =
    Random.uniform (Tile 2) [ Tile 4 ]



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
            span
                [ style "padding" "1em"
                ]
                [ text (String.fromInt number)
                ]

        Empty ->
            span
                [ style "padding" "1em"
                ]
                [ text "_"
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Slide keyDecoder)
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
