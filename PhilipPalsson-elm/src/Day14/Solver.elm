module Day14.Solver exposing (solve1, solve2)

import Binary
import Day14.Input
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


applyMask : Int -> String -> Int
applyMask value mask =
    List.map2
        (\b m ->
            if m == 'X' then
                b

            else if m == '1' then
                True

            else
                False
        )
        (Binary.fromDecimal value |> Binary.ensureSize 36 |> Binary.toBooleans)
        (String.toList mask)
        |> Binary.fromBooleans
        |> Binary.toDecimal


solve1 : String
solve1 =
    Day14.Input.input
        |> String.lines
        |> List.foldl
            (\action ( mask, memory ) ->
                case String.split " = " action of
                    "mask" :: newMask :: [] ->
                        ( newMask, memory )

                    memoryAddress :: value :: [] ->
                        ( mask
                        , Dict.insert
                            memoryAddress
                            (applyMask (value |> String.toInt |> Maybe.withDefault 0) mask)
                            memory
                        )

                    _ ->
                        ( mask, memory )
            )
            ( "", Dict.empty )
        |> Tuple.second
        |> Dict.values
        |> List.sum
        |> String.fromInt


calculateMemoryAddressList : String -> List Bool -> List Bool -> List Int
calculateMemoryAddressList mask bitsDone bitsTodo =
    case bitsTodo of
        head :: tail ->
            if String.startsWith "0" mask then
                calculateMemoryAddressList (String.dropLeft 1 mask) (head :: bitsDone) tail

            else if String.startsWith "1" mask then
                calculateMemoryAddressList (String.dropLeft 1 mask) (True :: bitsDone) tail

            else
                calculateMemoryAddressList (String.dropLeft 1 mask) (True :: bitsDone) tail
                    ++ calculateMemoryAddressList (String.dropLeft 1 mask) (False :: bitsDone) tail

        _ ->
            [ bitsDone |> List.reverse |> Binary.fromBooleans |> Binary.toDecimal ]


solve2 : String
solve2 =
    Day14.Input.input
        |> String.lines
        |> List.foldl
            (\action ( mask, memory ) ->
                case String.split " = " action of
                    "mask" :: newMask :: [] ->
                        ( newMask, memory )

                    memoryAddress :: value :: [] ->
                        ( mask
                        , memoryAddress
                            |> String.replace "mem[" ""
                            |> String.replace "]" ""
                            |> String.toInt
                            |> Maybe.withDefault 0
                            |> Binary.fromDecimal
                            |> Binary.ensureSize 36
                            |> Binary.toBooleans
                            |> calculateMemoryAddressList mask []
                            |> List.foldl (\address mem -> Dict.insert address value mem) memory
                        )

                    _ ->
                        ( mask, memory )
            )
            ( "", Dict.empty )
        |> Tuple.second
        |> Dict.values
        |> List.filterMap String.toInt
        |> List.sum
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
