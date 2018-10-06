module Main exposing (Direction(..), Food, Map, Model, Position, Row, Snake, Tile(..), main)

import Browser exposing (..)
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Random exposing (..)
import Time exposing (..)


type alias Position =
    ( Int, Int )


type alias Snake =
    { head : Position
    , tail : List Position
    , growing : Bool
    }


mapSize : Int
mapSize =
    25


initialSnake : Snake
initialSnake =
    { head = ( mapSize // 2, mapSize // 2 )
    , tail = []
    , growing = False
    }


myMap : Map
myMap =
    createMap mapSize


initialModel : Model
initialModel =
    { snake = initialSnake
    , direction = Down
    , map = myMap
    , food = Nothing
    , dead = False
    }


type Tile
    = Wall Position
    | Open Position


type alias Row =
    List Tile


type alias Map =
    List Row


type alias Food =
    Maybe Position


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { snake : Snake
    , direction : Direction
    , map : Map
    , food : Food
    , dead : Bool
    }


type Msg
    = Tick Posix
    | KeyPress KeyDirection
    | GenerateFood Position


createMap : Int -> Map
createMap size =
    List.map (createRow size) (List.range 1 size)


createRow : Int -> Int -> Row
createRow size y =
    List.map (\x -> createTileAt size ( x, y )) (List.range 1 size)


createTileAt : Int -> Position -> Tile
createTileAt size ( x, y ) =
    if x == 1 || y == 1 || x == size || y == size then
        Wall ( x, y )

    else
        Open ( x, y )


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type KeyDirection
    = LeftArrow
    | RightArrow
    | UpArrow
    | DownArrow
    | OtherKey


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    let
        _ =
            Debug.log "key" string
    in
    case string of
        "ArrowLeft" ->
            KeyPress LeftArrow

        "ArrowRight" ->
            KeyPress RightArrow

        "ArrowUp" ->
            KeyPress UpArrow

        "ArrowDown" ->
            KeyPress DownArrow

        _ ->
            KeyPress OtherKey


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 200 Tick, onKeyDown keyDecoder ]


foodGenerator : Generator Position
foodGenerator =
    Random.pair (Random.int 1 mapSize) (Random.int 1 mapSize)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newSnake =
                    let
                        snake =
                            model.snake

                        ( headX, headY ) =
                            snake.head
                    in
                    case model.direction of
                        Left ->
                            { snake | head = ( headX - 1, headY ) }

                        Right ->
                            { snake | head = ( headX + 1, headY ) }

                        Up ->
                            { snake | head = ( headX, headY - 1 ) }

                        Down ->
                            { snake | head = ( headX, headY + 1 ) }

                food =
                    case model.food of
                        Nothing ->
                            Nothing

                        Just pos ->
                            if isFood model newSnake.head then
                                Nothing

                            else
                                model.food
            in
            ( { model | snake = newSnake, food = food }, generate GenerateFood foodGenerator )

        KeyPress key ->
            case key of
                UpArrow ->
                    ( { model | direction = Up }, Cmd.none )

                DownArrow ->
                    ( { model | direction = Down }, Cmd.none )

                LeftArrow ->
                    ( { model | direction = Left }, Cmd.none )

                RightArrow ->
                    ( { model | direction = Right }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenerateFood pos ->
            case model.food of
                Nothing ->
                    ( { model | food = Just pos }, Cmd.none )

                Just food ->
                    ( { model | food = Just food }, Cmd.none )


isSnake : Snake -> Position -> Bool
isSnake snake ( x, y ) =
    let
        ( snakeX, snakeY ) =
            snake.head
    in
    x == snakeX && y == snakeY


isFood : Model -> Position -> Bool
isFood model ( x, y ) =
    case model.food of
        Nothing ->
            False

        Just ( fX, fY ) ->
            x == fX && y == fY


viewTile : Model -> Tile -> Html Msg
viewTile model tile =
    case tile of
        Wall position ->
            let
                className =
                    if isSnake model.snake position then
                        "snake"

                    else if isFood model position then
                        "food"

                    else
                        "wall"
            in
            span [ class ("tile " ++ className) ] []

        Open position ->
            let
                className =
                    if isSnake model.snake position then
                        "snake"

                    else if isFood model position then
                        "food"

                    else
                        "wall"
            in
            span [ class ("tile " ++ className) ] []


viewRow : Model -> Row -> Html Msg
viewRow model row =
    div [ class "row" ] (List.map (viewTile model) row)


viewMap : Model -> Html Msg
viewMap model =
    div [ class "map" ] (List.map (viewRow model) model.map)


view : Model -> Html Msg
view model =
    viewMap model


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
