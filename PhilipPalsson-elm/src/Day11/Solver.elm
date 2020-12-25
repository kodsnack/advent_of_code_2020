module Day11.Solver exposing (solve1, solve2)

import Array exposing (Array)
import Day11.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type Position
    = Floor
    | Empty
    | Occupied


type alias Positions =
    Array (Array Position)


toPosition : String -> Position
toPosition string =
    case string of
        "." ->
            Floor

        "L" ->
            Empty

        _ ->
            Occupied


neighborOffset =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]


occupiedAdjacentPositions : Positions -> Int -> Int -> Int
occupiedAdjacentPositions positions x y =
    neighborOffset
        |> List.foldl
            (\( dx, dy ) occupiedCount ->
                if getValue positions (x + dx) (y + dy) == Just Occupied then
                    occupiedCount + 1

                else
                    occupiedCount
            )
            0


iterate1 : Positions -> Int
iterate1 positions =
    let
        newPositions =
            Array.indexedMap
                (\y rows ->
                    Array.indexedMap
                        (\x position ->
                            let
                                occupiedCount =
                                    occupiedAdjacentPositions positions x y
                            in
                            if position == Empty && occupiedCount == 0 then
                                Occupied

                            else if position == Occupied && occupiedCount >= 4 then
                                Empty

                            else
                                position
                        )
                        rows
                )
                positions
    in
    if newPositions == positions then
        newPositions
            |> Array.map (Array.filter ((==) Occupied) >> Array.length)
            |> Array.toList
            |> List.sum

    else
        iterate1 newPositions


getValue : Array (Array Position) -> Int -> Int -> Maybe Position
getValue positions x y =
    Array.get y positions |> Maybe.andThen (Array.get x)


findOccupied : Positions -> Int -> Int -> Int -> Int -> Bool
findOccupied positions x y dx dy =
    case getValue positions (x + dx) (y + dy) of
        Just Occupied ->
            True

        Just Empty ->
            False

        Just Floor ->
            findOccupied positions (x + dx) (y + dy) dx dy

        Nothing ->
            False


occupiedAdjacentPositions2 : Positions -> Int -> Int -> Int
occupiedAdjacentPositions2 positions x y =
    neighborOffset
        |> List.foldl
            (\( dx, dy ) occupiedCount ->
                if findOccupied positions x y dx dy then
                    occupiedCount + 1

                else
                    occupiedCount
            )
            0


iterate2 : Positions -> Int
iterate2 positions =
    let
        newPositions =
            Array.indexedMap
                (\y rows ->
                    Array.indexedMap
                        (\x position ->
                            let
                                occupiedCount =
                                    occupiedAdjacentPositions2 positions x y
                            in
                            if position == Empty && occupiedCount == 0 then
                                Occupied

                            else if position == Occupied && occupiedCount >= 5 then
                                Empty

                            else
                                position
                        )
                        rows
                )
                positions
    in
    if newPositions == positions then
        newPositions
            |> Array.map (Array.filter ((==) Occupied) >> Array.length)
            |> Array.toList
            |> List.sum

    else
        iterate2 newPositions


solve1 : String
solve1 =
    Day11.Input.input
        |> String.lines
        |> List.map (String.split "" >> List.map toPosition >> Array.fromList)
        |> Array.fromList
        |> iterate1
        |> String.fromInt


solve2 : String
solve2 =
    Day11.Input.input
        |> String.lines
        |> List.map (String.split "" >> List.map toPosition >> Array.fromList)
        |> Array.fromList
        |> iterate2
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
