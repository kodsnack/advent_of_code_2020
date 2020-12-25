module Day13.Solver exposing (..)

import Day13.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


joinTuple : (a -> b -> c) -> ( a, b ) -> c
joinTuple func ( a, b ) =
    func a b


helper1 : Int -> List Int -> Int
helper1 timestamp bussIds =
    List.map (\bussId -> ( bussId, bussId - modBy bussId timestamp )) bussIds
        |> List.Extra.minimumBy Tuple.second
        |> Maybe.map (joinTuple (*))
        |> Maybe.withDefault 0


solve1 : String
solve1 =
    case Day13.Input.input |> String.lines of
        timestampString :: bussIdsString :: [] ->
            helper1
                (String.toInt timestampString |> Maybe.withDefault 0)
                (String.split "," bussIdsString |> List.filterMap String.toInt)
                |> String.fromInt

        _ ->
            "Invalid input"


findSolution : List ( Int, Int ) -> Int -> Int -> Int
findSolution bussIdsWithOffset time stepSize =
    if List.all (\( offset, bussId ) -> modBy bussId (time + offset) == 0) bussIdsWithOffset then
        time

    else
        findSolution bussIdsWithOffset (time + stepSize) stepSize


solve2 : String
solve2 =
    case Day13.Input.input |> String.lines of
        _ :: bussIdsString :: [] ->
            String.split "," bussIdsString
                |> List.indexedMap Tuple.pair
                |> List.filterMap (\( i, v ) -> String.toInt v |> Maybe.map (Tuple.pair i))
                |> List.Extra.inits
                |> List.drop 1
                |> List.foldl
                    (\lst ( prevSolution, stepSize ) ->
                        ( findSolution lst prevSolution stepSize
                        , stepSize * (lst |> List.reverse |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault 0)
                        )
                    )
                    ( 0, 1 )
                |> Tuple.first
                |> String.fromInt

        _ ->
            "Invalid input"


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
