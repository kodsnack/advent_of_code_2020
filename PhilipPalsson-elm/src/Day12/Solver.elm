module Day12.Solver exposing (solve1, solve2)

import Day12.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type Action
    = North Int
    | East Int
    | South Int
    | West Int
    | Left Int
    | Right Int
    | Forward Int


stringToAction : String -> Action
stringToAction string =
    let
        toInt =
            String.toInt >> Maybe.withDefault 0
    in
    case ( String.left 1 string, String.dropLeft 1 string ) of
        ( "N", value ) ->
            North (toInt value)

        ( "E", value ) ->
            East (toInt value)

        ( "S", value ) ->
            South (toInt value)

        ( "W", value ) ->
            West (toInt value)

        ( "L", value ) ->
            Left (toInt value)

        ( "R", value ) ->
            Right (toInt value)

        ( "F", value ) ->
            Forward (toInt value)

        _ ->
            Forward 0


type alias PositionWithRotation =
    { x : Int, y : Int, r : Int }


doAction : Action -> PositionWithRotation -> PositionWithRotation
doAction action position =
    case action of
        North int ->
            { position | y = position.y + int }

        East int ->
            { position | x = position.x + int }

        South int ->
            { position | y = position.y - int }

        West int ->
            { position | x = position.x - int }

        Left int ->
            { position | r = position.r + int }

        Right int ->
            { position | r = position.r - int }

        Forward int ->
            case modBy 360 position.r of
                0 ->
                    doAction (East int) position

                90 ->
                    doAction (North int) position

                180 ->
                    doAction (West int) position

                270 ->
                    doAction (South int) position

                _ ->
                    position


type alias Point =
    { x : Int, y : Int }


solve1 : String
solve1 =
    Day12.Input.input
        |> String.lines
        |> List.map stringToAction
        |> List.foldl doAction { x = 0, y = 0, r = 0 }
        |> (\p -> abs p.x + abs p.y)
        |> String.fromInt


move : ( Point, Point ) -> ( Point, Point )
move ( boatPosition, offset ) =
    ( { boatPosition
        | x = boatPosition.x + offset.x
        , y = boatPosition.y + offset.y
      }
    , offset
    )


rotateClockwise : Int -> Point -> Point
rotateClockwise degree ({ x, y } as waypointOffset) =
    case degree of
        90 ->
            { waypointOffset | x = y, y = x * -1 }

        180 ->
            { waypointOffset | x = x * -1, y = y * -1 }

        270 ->
            { waypointOffset | x = y * -1, y = x }

        _ ->
            waypointOffset


doAction2 : Action -> ( Point, Point ) -> ( Point, Point )
doAction2 action ( boatPosition, waypointOffset ) =
    case action of
        North int ->
            ( boatPosition, { waypointOffset | y = waypointOffset.y + int } )

        East int ->
            ( boatPosition, { waypointOffset | x = waypointOffset.x + int } )

        South int ->
            ( boatPosition, { waypointOffset | y = waypointOffset.y - int } )

        West int ->
            ( boatPosition, { waypointOffset | x = waypointOffset.x - int } )

        Left int ->
            ( boatPosition, rotateClockwise (360 - int) waypointOffset )

        Right int ->
            ( boatPosition, rotateClockwise int waypointOffset )

        Forward int ->
            List.foldl
                (\_ offset -> move offset)
                ( boatPosition, waypointOffset )
                (List.range 1 int)


solve2 : String
solve2 =
    Day12.Input.input
        |> String.lines
        |> List.map stringToAction
        |> List.foldl doAction2 ( { x = 0, y = 0 }, { x = 10, y = 1 } )
        |> (\( boatPosition, _ ) -> abs boatPosition.x + abs boatPosition.y)
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
