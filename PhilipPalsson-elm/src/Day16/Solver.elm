module Day16.Solver exposing (solve1, solve2)

import Day16.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra as List


toInt : String -> Int
toInt =
    String.toInt >> Maybe.withDefault 0


type alias Rule =
    { aStart : Int, aEnd : Int, bStart : Int, bEnd : Int, isDepartment : Bool }


parseRule : String -> Rule
parseRule ruleString =
    let
        isDeparture =
            String.split ": " ruleString
                |> List.head
                |> Maybe.map (String.startsWith "departure")
                |> Maybe.withDefault False
    in
    String.split ": " ruleString
        |> List.drop 1
        |> List.head
        |> Maybe.map
            (\s ->
                case String.split " or " s |> List.map (String.split "-") of
                    [ aStart, aEnd ] :: [ bStart, bEnd ] :: [] ->
                        { aStart = toInt aStart
                        , aEnd = toInt aEnd
                        , bStart = toInt bStart
                        , bEnd = toInt bEnd
                        , isDepartment = isDeparture
                        }

                    _ ->
                        { aStart = 0, aEnd = 0, bStart = 0, bEnd = 0, isDepartment = False }
            )
        |> Maybe.withDefault { aStart = 0, aEnd = 0, bStart = 0, bEnd = 0, isDepartment = False }


matchesRule : Int -> Rule -> Bool
matchesRule number { aStart, aEnd, bStart, bEnd } =
    (number >= aStart && number <= aEnd) || (number >= bStart && number <= bEnd)


invalidNumbers : List Rule -> List Int -> List Int
invalidNumbers rules numbers =
    List.filter
        (\number ->
            if List.any (matchesRule number) rules then
                False

            else
                True
        )
        numbers


solve1 : String
solve1 =
    case Day16.Input.input |> String.split "\n\n" of
        rulesString :: _ :: nearbyTicketsString :: [] ->
            let
                rules =
                    String.lines rulesString
                        |> List.map parseRule
            in
            String.lines nearbyTicketsString
                |> List.drop 1
                |> List.map (String.split "," >> List.filterMap String.toInt)
                |> List.map (invalidNumbers rules >> List.sum)
                |> List.sum
                |> String.fromInt

        _ ->
            "Error in input"


switchRowsAndColumns : List (List a) -> List (List a)
switchRowsAndColumns =
    List.foldl
        (\row result ->
            if List.isEmpty result then
                List.groupsOf 1 row

            else
                List.map2 (\a b -> a ++ [ b ]) result row
        )
        []


solve2 : String
solve2 =
    case Day16.Input.input |> String.split "\n\n" of
        rulesString :: myTicketString :: nearbyTicketsString :: [] ->
            let
                rules =
                    String.lines rulesString
                        |> List.map parseRule

                rulesOrdered =
                    String.lines nearbyTicketsString
                        |> List.drop 1
                        |> List.map (String.split "," >> List.filterMap String.toInt)
                        |> List.filter (\numbers -> List.length (invalidNumbers rules numbers) == 0)
                        |> switchRowsAndColumns
                        |> List.foldl
                            (\numbers ( remainingRules, res ) ->
                                case
                                    remainingRules
                                        |> List.filter (\rule -> List.all (\number -> matchesRule number rule) numbers)
                                of
                                    matchingRule :: [] ->
                                        ( List.remove matchingRule remainingRules, res ++ [ [ matchingRule ] ] )

                                    matchingRules ->
                                        ( remainingRules, res ++ [ matchingRules ] )
                            )
                            ( rules, [] )
                        |> decideRuleOrders
            in
            List.map2
                (\number { isDepartment } ->
                    if isDepartment then
                        Just number

                    else
                        Nothing
                )
                (String.lines myTicketString
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (String.split "," >> List.filterMap String.toInt)
                    |> Maybe.withDefault []
                )
                rulesOrdered
                |> List.filterMap identity
                |> List.product
                |> String.fromInt

        _ ->
            "Error in input"


decideRuleOrders : ( List Rule, List (List Rule) ) -> List Rule
decideRuleOrders ( rulesLeft, res ) =
    if List.length rulesLeft == 0 then
        List.map List.head res |> List.filterMap identity

    else
        List.mapAccuml
            (\left rules ->
                case rules of
                    onlyRule :: [] ->
                        ( List.remove onlyRule left, rules )

                    _ ->
                        case List.filter (\rule -> List.member rule left) rules of
                            onlyValidRule :: [] ->
                                ( List.remove onlyValidRule left, [ onlyValidRule ] )

                            validRules ->
                                ( left, validRules )
            )
            rulesLeft
            res
            |> decideRuleOrders


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
