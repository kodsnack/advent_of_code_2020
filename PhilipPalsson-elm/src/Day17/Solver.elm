module Day17.Solver exposing (solve1, solve2)

import Day17.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra
import Set exposing (Set)


type alias Cell1 =
    ( Int, Int, Int )


type alias Cell2 =
    ( ( Int, Int ), ( Int, Int ) )


neighbors1 : Cell1 -> Set Cell1
neighbors1 (( x, y, z ) as cell) =
    List.map
        (\i ->
            ( (x - 1) + modBy 3 i
            , (y - 1) + modBy 3 (i // 3)
            , (z - 1) + modBy 3 (i // 9)
            )
        )
        (List.range 0 81)
        |> Set.fromList
        |> Set.remove cell


neighbors2 : Cell2 -> Set Cell2
neighbors2 (( ( x, y ), ( z, w ) ) as cell) =
    List.map
        (\i ->
            ( ( (x - 1) + modBy 3 i
              , (y - 1) + modBy 3 (i // 3)
              )
            , ( (z - 1) + modBy 3 (i // 9)
              , (w - 1) + modBy 3 (i // 27)
              )
            )
        )
        (List.range 0 81)
        |> Set.fromList
        |> Set.remove cell


cycle : (comparable -> Set comparable) -> Set comparable -> Set comparable
cycle neighbors active =
    let
        allToCheck =
            active
                |> Set.foldl
                    (\cell acc ->
                        Set.union acc (neighbors cell)
                    )
                    active

        neighborActiveCount cell =
            neighbors cell
                |> Set.filter (\n -> Set.member n active)
                |> Set.size
    in
    Set.filter
        (\c ->
            let
                count =
                    neighborActiveCount c
            in
            if Set.member c active && (count == 2 || count == 3) then
                True

            else if not (Set.member c active) && count == 3 then
                True

            else
                False
        )
        allToCheck


solver : (Int -> Int -> comparable) -> (comparable -> Set comparable) -> String
solver init neighbors =
    Day17.Input.input
        |> String.lines
        |> List.map String.toList
        |> List.Extra.indexedFoldl
            (\y row res ->
                List.Extra.indexedFoldl
                    (\x char innerRes ->
                        if char == '#' then
                            Set.insert (init x y) innerRes

                        else
                            innerRes
                    )
                    res
                    row
            )
            Set.empty
        |> cycle neighbors
        |> cycle neighbors
        |> cycle neighbors
        |> cycle neighbors
        |> cycle neighbors
        |> cycle neighbors
        |> Set.size
        |> String.fromInt


solve1 : String
solve1 =
    solver (\x y -> ( x, y, 0 )) neighbors1


solve2 : String
solve2 =
    solver (\x y -> ( ( x, y ), ( 0, 0 ) )) neighbors2


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
