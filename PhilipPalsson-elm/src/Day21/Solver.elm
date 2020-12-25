module Day21.Solver exposing (solve1, solve2)

import Day21.Input
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra
import Set exposing (Set)


type alias Food =
    { ingredients : Set String, allergens : Set String }


parseFood : String -> Food
parseFood string =
    case String.split " (contains " string of
        ingredientsString :: [ allergensString ] ->
            { ingredients = String.split " " ingredientsString |> Set.fromList
            , allergens =
                String.replace ")" "" allergensString
                    |> String.split ", "
                    |> Set.fromList
            }

        _ ->
            { ingredients = Set.empty
            , allergens = Set.empty
            }


input =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""


countIngredients : List Food -> Dict String Int
countIngredients foods =
    foods
        |> List.map (\f -> f.ingredients |> Set.toList |> String.join " ")
        |> String.join " "
        |> String.split " "
        |> List.sort
        |> List.Extra.groupWhile (==)
        |> List.map (\( name, lst ) -> ( name, 1 + List.length lst ))
        |> Dict.fromList


narrowDownIngredientPossibilities : Dict String (Set String) -> Dict String (Set String)
narrowDownIngredientPossibilities allergensWithPossibleIngredients =
    let
        allergensWithOnly1PossibleIngredient =
            Dict.toList allergensWithPossibleIngredients
                |> List.map Tuple.second
                |> List.filter (Set.size >> (==) 1)
                |> List.foldl Set.union Set.empty

        res =
            Dict.map
                (\_ ingredients ->
                    if Set.size ingredients == 1 then
                        ingredients

                    else
                        Set.diff ingredients allergensWithOnly1PossibleIngredient
                )
                allergensWithPossibleIngredients
    in
    if res == allergensWithPossibleIngredients then
        res

    else
        narrowDownIngredientPossibilities res


getAllergensWithTheirIngredient : List Food -> Dict String String
getAllergensWithTheirIngredient foods =
    List.foldl
        (\food acc ->
            Set.foldl
                (\allergen innerAcc ->
                    case Dict.get allergen innerAcc of
                        Just setOfPossibleIngredients ->
                            Dict.insert
                                allergen
                                (Set.intersect food.ingredients setOfPossibleIngredients)
                                innerAcc

                        Nothing ->
                            Dict.insert
                                allergen
                                food.ingredients
                                innerAcc
                )
                acc
                food.allergens
        )
        Dict.empty
        foods
        |> narrowDownIngredientPossibilities
        |> Dict.map
            (\_ ingredients ->
                Set.toList ingredients
                    |> List.head
                    |> Maybe.withDefault ""
            )


solve1 : Int
solve1 =
    let
        foods =
            Day21.Input.input
                |> String.lines
                |> List.map parseFood

        ingredientCount =
            countIngredients foods

        ingredientsWithAllergen =
            Dict.toList (getAllergensWithTheirIngredient foods)
                |> List.map Tuple.second
                |> Set.fromList
    in
    Set.foldl Dict.remove ingredientCount ingredientsWithAllergen
        |> Dict.foldl (\_ c acc -> acc + c) 0


solve2 : String
solve2 =
    let
        foods =
            Day21.Input.input
                |> String.lines
                |> List.map parseFood
    in
    getAllergensWithTheirIngredient foods
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> String.join ","


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
