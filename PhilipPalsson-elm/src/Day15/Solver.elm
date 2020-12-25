module Day15.Solver exposing (solve1, solve2)

import Day15.Input
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


solve n =
    let
        startingNumbers : List Int
        startingNumbers =
            Day15.Input.input
                |> String.split ","
                |> List.filterMap String.toInt

        startingPrevNumber : Int
        startingPrevNumber =
            List.reverse startingNumbers
                |> List.head
                |> Maybe.withDefault 0

        startingState : Dict.Dict Int ( Int, Maybe Int )
        startingState =
            List.Extra.indexedFoldl
                (\index number acc ->
                    Dict.insert number ( index + 1, Nothing ) acc
                )
                Dict.empty
                startingNumbers

        updateState : Int -> Int -> Dict.Dict Int ( Int, Maybe Int ) -> Dict.Dict Int ( Int, Maybe Int )
        updateState index number state =
            case Dict.get number state of
                Just ( lastTime, Just _ ) ->
                    Dict.insert number ( index, Just lastTime ) state

                Just ( lastTime, Nothing ) ->
                    Dict.insert number ( index, Just lastTime ) state

                Nothing ->
                    Dict.insert number ( index, Nothing ) state

        helper : Int -> ( Int, Dict.Dict Int ( Int, Maybe Int ) ) -> ( Int, Dict.Dict Int ( Int, Maybe Int ) )
        helper index ( prevNumber, state ) =
            case Dict.get prevNumber state of
                Just ( lastTime, Just lastLastTime ) ->
                    let
                        numb =
                            lastTime - lastLastTime
                    in
                    ( numb, updateState index numb state )

                Just ( _, Nothing ) ->
                    ( 0, updateState index 0 state )

                Nothing ->
                    ( prevNumber, state )
    in
    List.foldl
        helper
        ( startingPrevNumber, startingState )
        (List.range (List.length startingNumbers + 1) n)
        |> Tuple.first
        |> String.fromInt


solve1 : String
solve1 =
    solve 2020


solve2 : String
solve2 =
    solve 30000000


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
