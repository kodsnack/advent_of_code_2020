module Day5.Solver exposing (solve1, solve2)

import Day5.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


type alias Seat =
    { fromRow : Int, toRow : Int, fromCol : Int, toCol : Int }


diff : Int -> Int -> Int
diff from to =
    ceiling (toFloat (to - from) / 2)


narrowDownSeat : String -> Seat -> Seat
narrowDownSeat s seat =
    if s == "F" then
        { seat | toRow = seat.toRow - diff seat.fromRow seat.toRow }

    else if s == "B" then
        { seat | fromRow = seat.fromRow + diff seat.fromRow seat.toRow }

    else if s == "L" then
        { seat | toCol = seat.toCol - diff seat.fromCol seat.toCol }

    else if s == "R" then
        { seat | fromCol = seat.fromCol + diff seat.fromCol seat.toCol }

    else
        seat


toSeatId : Seat -> Int
toSeatId seat =
    (seat.toRow * 8) + seat.toCol


solve1 : String
solve1 =
    Day5.Input.input
        |> String.split "\n"
        |> List.map (String.split "" >> List.foldl narrowDownSeat (Seat 0 127 0 7))
        |> List.map toSeatId
        |> List.maximum
        |> Maybe.withDefault 0
        |> String.fromInt


solve2 : String
solve2 =
    let
        seatIds =
            Day5.Input.input
                |> String.split "\n"
                |> List.map (String.split "" >> List.foldl narrowDownSeat (Seat 0 127 0 7))
                |> List.map toSeatId
                |> List.sort

        offset =
            List.head seatIds |> Maybe.withDefault 0
    in
    List.indexedMap Tuple.pair seatIds
        |> List.Extra.findIndex (\( index, seatId ) -> seatId /= index + offset)
        |> Maybe.map ((+) offset)
        |> Maybe.withDefault 0
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
