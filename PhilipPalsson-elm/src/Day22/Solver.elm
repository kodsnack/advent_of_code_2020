module Day22.Solver exposing (solve1, solve2)

import Day22.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Set


parseInput =
    case Day22.Input.input |> String.split "\n\n" of
        player1String :: [ player2String ] ->
            ( String.lines player1String |> List.drop 1 |> List.filterMap String.toInt
            , String.lines player2String |> List.drop 1 |> List.filterMap String.toInt
            )

        _ ->
            ( [], [] )


calculateScore : List Int -> Int
calculateScore values =
    values |> List.reverse |> List.indexedMap (\index value -> (index + 1) * value) |> List.sum


type Winner
    = Player1 (List Int)
    | Player2 (List Int)


handlePart2 seenBefore player1 player2 =
    if Set.member ( player1, player2 ) seenBefore then
        Player1 player1

    else
        case ( player1, player2 ) of
            ( player1First :: player1Rest, player2First :: player2Rest ) ->
                if List.length player1Rest >= player1First && List.length player2Rest >= player2First then
                    case handlePart2 Set.empty (List.take player1First player1Rest) (List.take player2First player2Rest) of
                        Player1 _ ->
                            handlePart2
                                (Set.insert ( player1, player2 ) seenBefore)
                                (player1Rest ++ [ player1First, player2First ])
                                player2Rest

                        Player2 _ ->
                            handlePart2
                                (Set.insert ( player1, player2 ) seenBefore)
                                player1Rest
                                (player2Rest ++ [ player2First, player1First ])

                else if player1First > player2First then
                    handlePart2
                        (Set.insert ( player1, player2 ) seenBefore)
                        (player1Rest ++ [ player1First, player2First ])
                        player2Rest

                else
                    handlePart2
                        (Set.insert ( player1, player2 ) seenBefore)
                        player1Rest
                        (player2Rest ++ [ player2First, player1First ])

            ( [], player2All ) ->
                Player2 player2All

            ( player1All, [] ) ->
                Player1 player1All


handlePart1 : List Int -> List Int -> Int
handlePart1 player1 player2 =
    case ( player1, player2 ) of
        ( player1First :: player1Rest, player2First :: player2Rest ) ->
            if player1First > player2First then
                handlePart1 (player1Rest ++ [ player1First, player2First ]) player2Rest

            else
                handlePart1 player1Rest (player2Rest ++ [ player2First, player1First ])

        ( [], player2All ) ->
            calculateScore player2All

        ( player1All, [] ) ->
            calculateScore player1All


solve1 : Int
solve1 =
    let
        ( player1, player2 ) =
            parseInput
    in
    handlePart1 player1 player2


solve2 : Int
solve2 =
    let
        ( player1, player2 ) =
            parseInput
    in
    case handlePart2 Set.empty player1 player2 of
        Player1 values ->
            calculateScore values

        Player2 values ->
            calculateScore values


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
