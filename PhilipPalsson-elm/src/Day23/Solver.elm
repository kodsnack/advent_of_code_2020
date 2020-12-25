module Day23.Solver exposing (solve1, solve2)

import Day23.Input
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


findDestination : Int -> Int -> List Int -> Int
findDestination maxValue value pickedUp =
    if value == 0 then
        findDestination maxValue maxValue pickedUp

    else if List.member value pickedUp then
        findDestination maxValue (value - 1) pickedUp

    else
        value


doAMove : Int -> ( Int, Dict Int Int ) -> ( Int, Dict Int Int )
doAMove maxValue ( current, values ) =
    let
        firstPickup =
            Dict.get current values |> Maybe.withDefault 0

        secondPickup =
            Dict.get firstPickup values |> Maybe.withDefault 0

        thirdPickup =
            Dict.get secondPickup values |> Maybe.withDefault 0

        afterPickup =
            Dict.get thirdPickup values |> Maybe.withDefault 0

        destination =
            findDestination maxValue (current - 1) [ firstPickup, secondPickup, thirdPickup ]

        afterDestination =
            Dict.get destination values |> Maybe.withDefault 0

        updatedValues =
            values
                |> Dict.insert destination firstPickup
                |> Dict.insert thirdPickup afterDestination
                |> Dict.insert current afterPickup
    in
    ( afterPickup, updatedValues )


stateToList : Int -> List Int -> Dict Int Int -> List Int
stateToList current acc values =
    case Dict.get current values of
        Just next ->
            stateToList next (acc ++ [ next ]) (Dict.remove current values)

        Nothing ->
            acc


buildInitialState : List Int -> ( Int, Dict Int Int )
buildInitialState values =
    List.foldr
        (\val ( prev, res ) ->
            ( val, Dict.insert val prev res )
        )
        ( List.head values |> Maybe.withDefault 0, Dict.empty )
        values


solve1 : String
solve1 =
    let
        initialState =
            Day23.Input.input
                |> String.split ""
                |> List.filterMap String.toInt
                |> buildInitialState
    in
    List.foldl (\_ acc -> doAMove 9 acc)
        initialState
        (List.range 1 100)
        |> Tuple.second
        |> stateToList 1 []
        |> List.Extra.remove 1
        |> List.map String.fromInt
        |> String.join ""


solve2 : Int
solve2 =
    let
        lst : List Int
        lst =
            Day23.Input.input
                |> String.split ""
                |> List.filterMap String.toInt

        initialState =
            buildInitialState (lst ++ List.range 10 1000000)

        valuesAfterLastRound =
            List.foldl (\_ acc -> doAMove 1000000 acc)
                initialState
                (List.range 1 10000000)
                |> Tuple.second

        firstValueAfter1 =
            Dict.get 1 valuesAfterLastRound |> Maybe.withDefault 0 |> Debug.log "first"

        secondValueAfter1 =
            Dict.get firstValueAfter1 valuesAfterLastRound |> Maybe.withDefault 0 |> Debug.log "second"
    in
    firstValueAfter1 * secondValueAfter1


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
