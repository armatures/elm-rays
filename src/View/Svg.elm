module View.Svg exposing (root)

import Html exposing (Html)
import Mouse
import Raycasting
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)
import Vectors exposing (..)


root : Walls -> Maybe Mouse.Position -> Html msg
root walls pos =
    case pos of
        Just position ->
            svg
                [ width "600"
                , height "600"
                ]
                [ drawRays walls position
                , drawWalls walls
                , drawCursor position
                ]

        _ ->
            svg
                [ width "600"
                , height "600"
                ]
                [ drawWalls walls
                ]


neighbours : List a -> List ( a, a )
neighbours xs =
    List.map2 (,)
        xs
        ((xs ++ xs)
            |> List.tail
            |> Maybe.withDefault []
        )


drawRays : Walls -> Mouse.Position -> Svg msg
drawRays walls position =
    g []
        (Raycasting.solveRays
            { x = toFloat position.x
            , y = toFloat position.y
            }
            walls
            |> List.sortBy (.vector >> .angle)
            |> neighbours
            |> List.map drawTriangle
        )


drawWalls : Walls -> Svg msg
drawWalls walls =
    g []
        (List.map drawLine walls)


drawLine : Line -> Svg msg
drawLine line =
    let
        lineStart =
            Vectors.start line

        lineEnd =
            Vectors.end line
    in
        Svg.line
            [ x1 (toString lineStart.x)
            , y1 (toString lineStart.y)
            , x2 (toString lineEnd.x)
            , y2 (toString lineEnd.y)
            , stroke "black"
            , strokeWidth "5"
            , strokeLinecap "round"
            ]
            []


drawTriangle : ( Line, Line ) -> Svg msg
drawTriangle ( a, b ) =
    let
        formatPoint point =
            toString point.x ++ "," ++ toString point.y
    in
        polygon
            [ fill "gold"
            , stroke "gold"
            , points
                (String.join " "
                    (List.map formatPoint
                        [ Vectors.start a
                        , Vectors.end a
                        , Vectors.end b
                        , Vectors.start b
                        ]
                    )
                )
            ]
            []


drawCursor : Mouse.Position -> Svg msg
drawCursor position =
    circle
        [ cx (toString position.x)
        , cy (toString position.y)
        , r "5"
        , fill "red"
        ]
        []
