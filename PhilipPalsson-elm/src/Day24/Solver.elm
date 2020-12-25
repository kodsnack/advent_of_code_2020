module Day24.Solver exposing (solve1, solve2)

import Day24.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Set exposing (Set)


traverse : ( Float, Float ) -> String -> ( Float, Float )
traverse ( x, y ) string =
    if String.startsWith "e" string then
        traverse ( x + 1, y ) (String.dropLeft 1 string)

    else if String.startsWith "se" string then
        traverse ( x + 0.5, y - 1 ) (String.dropLeft 2 string)

    else if String.startsWith "sw" string then
        traverse ( x - 0.5, y - 1 ) (String.dropLeft 2 string)

    else if String.startsWith "w" string then
        traverse ( x - 1, y ) (String.dropLeft 1 string)

    else if String.startsWith "nw" string then
        traverse ( x - 0.5, y + 1 ) (String.dropLeft 2 string)

    else if String.startsWith "ne" string then
        traverse ( x + 0.5, y + 1 ) (String.dropLeft 2 string)

    else
        ( x, y )


solve1 : Int
solve1 =
    Day24.Input.input
        |> String.lines
        |> List.map (traverse ( 0, 0 ))
        |> List.sort
        |> List.foldl
            (\pos acc ->
                if Set.member pos acc then
                    Set.remove pos acc

                else
                    Set.insert pos acc
            )
            Set.empty
        |> Set.size


getNeighbors : ( Float, Float ) -> Set ( Float, Float )
getNeighbors ( x, y ) =
    Set.fromList
        [ ( x + 1, y )
        , ( x + 0.5, y - 1 )
        , ( x - 0.5, y - 1 )
        , ( x - 1, y )
        , ( x - 0.5, y + 1 )
        , ( x + 0.5, y + 1 )
        ]


getBlackNeighborCount : Set ( Float, Float ) -> ( Float, Float ) -> Int
getBlackNeighborCount blackBricks pos =
    getNeighbors pos
        |> Set.filter (\neighborPos -> Set.member neighborPos blackBricks)
        |> Set.size


flipBricks : Set ( Float, Float ) -> Set ( Float, Float )
flipBricks blackBricks =
    let
        allNeighbor =
            Set.foldl (\pos acc -> Set.union acc (getNeighbors pos)) Set.empty blackBricks

        blackBricksToKeep =
            Set.foldl
                (\pos acc ->
                    let
                        blackNeighborCount =
                            getBlackNeighborCount blackBricks pos
                    in
                    if blackNeighborCount == 0 || blackNeighborCount > 2 then
                        acc

                    else
                        Set.insert pos acc
                )
                Set.empty
                blackBricks

        newBlackBricksFromNeighbors =
            Set.foldl
                (\pos acc ->
                    let
                        blackNeighborCount =
                            getBlackNeighborCount blackBricks pos
                    in
                    if blackNeighborCount == 2 then
                        Set.insert pos acc

                    else
                        acc
                )
                Set.empty
                allNeighbor
    in
    Set.union blackBricksToKeep newBlackBricksFromNeighbors


solve2 : Int
solve2 =
    let
        initial =
            Day24.Input.input
                |> String.lines
                |> List.map (traverse ( 0, 0 ))
                |> List.sort
                |> List.foldl
                    (\pos acc ->
                        if Set.member pos acc then
                            Set.remove pos acc

                        else
                            Set.insert pos acc
                    )
                    Set.empty
    in
    List.foldl
        (\i acc ->
            let
                _ =
                    Debug.log "i" i
            in
            flipBricks acc
        )
        initial
        (List.range 1 100)
        |> Set.size


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
