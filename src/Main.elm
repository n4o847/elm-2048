module Main exposing (main)

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
    , score : Int
    , over : Bool
    , won : Bool
    }


type alias Board =
    List Row


type alias Row =
    List Cell


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
    let
        emptyBoard =
            List.repeat 4 <| List.repeat 4 <| Empty
    in
    ( { board = emptyBoard
      , score = 0
      , over = False
      , won = False
      }
    , randomPositionedTiles 2 emptyBoard
        |> Random.generate PutMany
    )



-- UPDATE


type Msg
    = Slide Direction
    | Put ( Position, Cell )
    | PutMany (List ( Position, Cell ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide direction ->
            let
                ( slidedBoard, increase ) =
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
                , score = model.score + increase
              }
            , cmd
            )

        Put ( position, cell ) ->
            let
                newBoard =
                    setBoard position cell model.board
            in
            ( { model
                | board = newBoard
                , over = model.over || stuck newBoard
                , won = model.won || won newBoard
              }
            , Cmd.none
            )

        PutMany list ->
            ( { model
                | board = List.foldl (\( position, cell ) -> setBoard position cell) model.board list
              }
            , Cmd.none
            )


setBoard : Position -> Cell -> Board -> Board
setBoard ( i, j ) cell =
    let
        updateAt idx f =
            List.indexedMap
                (\idx_ ->
                    if idx_ == idx then
                        f

                    else
                        identity
                )
    in
    updateAt i <| updateAt j <| always cell


mergeCell : Cell -> Cell -> ( Cell, Int )
mergeCell cellX cellY =
    case cellX of
        Tile x ->
            case cellY of
                Tile y ->
                    ( Tile (x + y), x + y )

                Empty ->
                    ( Empty, 0 )

        Empty ->
            ( Empty, 0 )


type Accumulator
    = Waiting Cell (List Cell) Int
    | Done (List Cell) Int


accumulate : Cell -> Accumulator -> Accumulator
accumulate cell acc =
    if cell == Empty then
        acc

    else
        case acc of
            Waiting waiting done score ->
                if waiting == cell then
                    let
                        ( merged, increase ) =
                            mergeCell cell waiting
                    in
                    Done (merged :: done) (score + increase)

                else
                    Waiting cell (waiting :: done) score

            Done done score ->
                Waiting cell done score


slideRow : List Cell -> ( List Cell, Int )
slideRow row =
    let
        acc =
            List.foldr accumulate (Done [] 0) row

        ( newRow, score ) =
            case acc of
                Waiting waiting done s ->
                    ( waiting :: done, s )

                Done done s ->
                    ( done, s )
    in
    ( List.repeat (List.length row - List.length newRow) Empty ++ newRow
    , score
    )


transpose : List (List Cell) -> List (List Cell)
transpose matrix =
    List.foldr (List.map2 (::)) (List.repeat (List.length matrix) []) matrix


slideBoard : Direction -> Board -> ( Board, Int )
slideBoard direction board =
    case direction of
        Left ->
            board
                |> List.map List.reverse
                |> slideBoard Right
                |> Tuple.mapFirst (List.map List.reverse)

        Right ->
            board
                |> List.map slideRow
                |> List.unzip
                |> Tuple.mapSecond List.sum

        Up ->
            board
                |> transpose
                |> slideBoard Left
                |> Tuple.mapFirst transpose

        Down ->
            board
                |> transpose
                |> slideBoard Right
                |> Tuple.mapFirst transpose

        Other ->
            ( board, 0 )


emptyPositionList : Board -> List Position
emptyPositionList board =
    board
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


randomPositionedTiles : Int -> Board -> Random.Generator (List ( Position, Cell ))
randomPositionedTiles n board =
    let
        positionList =
            sample n (emptyPositionList board)

        tileList =
            Random.list n randomTile
    in
    Random.map2 (List.map2 Tuple.pair) positionList tileList


sample : Int -> List a -> Random.Generator (List a)
sample n list =
    if n <= 0 then
        Random.constant []

    else
        let
            indexedList =
                List.indexedMap Tuple.pair list
        in
        case indexedList of
            [] ->
                Random.constant []

            head :: tail ->
                Random.uniform head tail
                    |> Random.andThen
                        (\( index, element ) ->
                            let
                                omitted =
                                    List.take index list ++ List.drop (index + 1) list
                            in
                            Random.map ((::) element) (sample (n - 1) omitted)
                        )


stuck : Board -> Bool
stuck board =
    [ Left, Right, Up, Down ]
        |> List.all
            (\direction ->
                Tuple.first (slideBoard direction board)
                    == board
            )


won : Board -> Bool
won board =
    List.any (List.member (Tile 2048)) board



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Consolas"
        ]
        [ h1 [] [ text "2048 in Elm" ]
        , div [] [ viewScore model.score ]
        , div [] [ viewBoard model.board ]
        , div []
            (if model.over then
                [ text "Game over!" ]

             else
                []
            )
        , div []
            (if model.won then
                [ text "You win!" ]

             else
                []
            )
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div
        [ style "display" "inline-block"
        , style "margin" "10px"
        , style "padding" "5px"
        , style "border-radius" "10px"
        , style "background-color" "#bbb"
        ]
    <|
        List.map viewRow board


viewRow : Row -> Html Msg
viewRow row =
    div [] <|
        List.map viewCell row


viewCell : Cell -> Html Msg
viewCell cell =
    let
        cellStyle =
            [ style "display" "inline-block"
            , style "margin" "5px"
            , style "width" "50px"
            , style "height" "50px"
            , style "line-height" "50px"
            , style "border-radius" "5px"
            , style "font-size" "24px"
            , style "text-align" "center"
            , style "vertical-align" "middle"
            ]
    in
    case cell of
        Tile number ->
            div
                (cellStyle
                    ++ [ style "background-color" "#eee"
                       ]
                )
                [ text (String.fromInt number)
                ]

        Empty ->
            div
                (cellStyle
                    ++ [ style "background-color" "#ccc"
                       ]
                )
                []


viewScore : Int -> Html Msg
viewScore score =
    div
        [ style "display" "inline-block"
        , style "margin" "10px"
        , style "padding" "10px"
        , style "border-radius" "10px"
        , style "font-size" "20px"
        , style "background-color" "#bbb"
        ]
        [ text <| "Score: " ++ String.fromInt score ]



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
