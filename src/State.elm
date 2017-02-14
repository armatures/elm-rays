module State exposing (..)

import Mouse
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Random
import Vectors exposing (Line, lineBetween)
import Types exposing (..)
import List


initialModel : Model
initialModel =
    { walls =
        [ { position = { x = 0, y = 0 }, vector = { length = 600, angle = degrees 0 } }
        , { position = { x = 0, y = 600 }, vector = { length = 600, angle = degrees 0 } }
        , { position = { x = 0, y = 0 }, vector = { length = 600, angle = degrees 90 } }
        , { position = { x = 600, y = 0 }, vector = { length = 600, angle = degrees 90 } }
        ]
    , mouse = Nothing
    }


initialCmd : Cmd Msg
initialCmd =
    Random.generate NewWalls <| Random.list 6 randomWall


randomWall : Random.Generator Vectors.Line
randomWall =
    Random.map4
        (\x1 y1 x2 y2 ->
            lineBetween { x = x1, y = y1 } { x = x2, y = y2 }
        )
        (Random.float 10 590)
        (Random.float 10 590)
        (Random.float 10 590)
        (Random.float 10 590)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWalls walls ->
            ( { model | walls = model.walls ++ walls }
            , Cmd.none
            )

        Mouse mouse ->
            ( { model | mouse = Just mouse }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Mouse.moves Mouse
