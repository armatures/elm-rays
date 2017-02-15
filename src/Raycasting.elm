module Raycasting exposing (solveRays)

import List.Extra
import Types exposing (..)
import Vectors exposing (..)


solveRays : Position -> Walls -> List Line
solveRays position walls =
    walls
        |> List.concatMap (raysToEndpoints position)
        |> (++) (raysToIntersections position walls)
        |> List.filterMap (curtail walls)


raysToEndpoints : Position -> Line -> List Line
raysToEndpoints position line =
    let
        rayToStart =
            lineBetween position (start line)

        rayToEnd =
            lineBetween position (end line)

        delta =
            degrees 0.1
    in
        [ addToAngle delta rayToStart
        , addToAngle (delta * -1) rayToStart
        , addToAngle delta rayToEnd
        , addToAngle (delta * -1) rayToEnd
        ]


raysToIntersections : Position -> List Line -> List Line
raysToIntersections position lines =
    allIntersections lines
    |> List.map (lineBetween position)

allIntersections : List Line -> List Position
allIntersections lines =
    case lines of
        [] ->
            []
        _ :: [] ->
            []
        l :: ls ->
            (List.filterMap (\line -> intersection l line) ls)
            ++ allIntersections ls


intersection : Line -> Line -> Maybe Position
intersection l1 l2 =
    Maybe.map end (intersect l1 l2)


curtail : Walls -> Line -> Maybe Line
curtail walls line =
    walls
        |> List.filterMap (intersect line)
        |> List.Extra.minimumBy (.vector >> .length)


intersect : Line -> Line -> Maybe Line
intersect ray target =
    let
        rayStart =
            start ray

        wallStart =
            start target

        rayComponents =
            components ray

        targetComponents =
            components target

        targetLength =
            ((rayStart.x * rayComponents.dy)
                - (rayStart.y * rayComponents.dx)
                + (wallStart.y * rayComponents.dx)
                - (wallStart.x * rayComponents.dy)
            )
                / ((rayComponents.dy * targetComponents.dx)
                    - (rayComponents.dx * targetComponents.dy)
                  )

        rayLength =
            (wallStart.x - rayStart.x + targetComponents.dx * targetLength)
                / rayComponents.dx
    in
        if rayLength < 0 then
            Nothing
        else if targetLength < 0 || target.vector.length < targetLength then
            Nothing
        else
            Just (withLength rayLength ray)
