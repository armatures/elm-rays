module Types exposing (Walls, Model, Msg(..))

import Mouse
import Vectors exposing (..)


type alias Walls =
    List Line


type alias Model =
    { walls : Walls
    , mouse : Maybe Mouse.Position
    }


type Msg
    = Mouse Mouse.Position
    | NewWalls Walls
