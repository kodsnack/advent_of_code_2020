module Day7.Solver exposing (solve1, solve2)

import Day7.Input
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


parseBag : String -> ( String, List ( Int, String ) )
parseBag string =
    case String.split " bags contain " string of
        color :: bags :: [] ->
            ( color
            , if bags == "no other bags." then
                []

              else
                bags
                    |> String.replace " bags" ""
                    |> String.replace " bag" ""
                    |> String.replace "." ""
                    |> String.split ", "
                    |> List.map
                        (\bagString ->
                            ( String.left 1 bagString
                                |> String.toInt
                                |> Maybe.withDefault 0
                            , String.slice 2 (String.length bagString) bagString
                            )
                        )
            )

        _ ->
            ( "", [] )


canHaveShinyGold : Dict String (List ( Int, String )) -> List ( Int, String ) -> Bool
canHaveShinyGold allBags =
    List.any
        (\( _, color ) ->
            color
                == "shiny gold"
                || (Dict.get color allBags |> Maybe.map (canHaveShinyGold allBags) |> Maybe.withDefault False)
        )


numberOfBags : Dict String (List ( Int, String )) -> List ( Int, String ) -> Int
numberOfBags allBags =
    List.map
        (\( count, color ) ->
            let
                _ =
                    Debug.log ""
                        { color = color
                        , count = count
                        , extra = max 1 (Dict.get color allBags |> Maybe.map (numberOfBags allBags) |> Maybe.withDefault 0)
                        }
            in
            count + (count * (Dict.get color allBags |> Maybe.map (numberOfBags allBags) |> Maybe.withDefault 0))
        )
        >> List.sum


solve1 : String
solve1 =
    let
        bags =
            Day7.Input.input
                |> String.split "\n"
                |> List.map parseBag
                |> Dict.fromList

        count =
            Dict.foldl
                (\_ v c ->
                    if canHaveShinyGold bags v then
                        c + 1

                    else
                        c
                )
                0
                bags
    in
    String.fromInt count


solve2 : String
solve2 =
    let
        bags =
            Day7.Input.input
                |> String.split "\n"
                |> List.map parseBag
                |> Dict.fromList
    in
    String.fromInt (Dict.get "shiny gold" bags |> Maybe.map (numberOfBags bags) |> Maybe.withDefault 0)


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
