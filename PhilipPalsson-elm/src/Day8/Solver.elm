module Day8.Solver exposing (solve1, solve2)

import Array exposing (Array)
import Day8.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra
import Set exposing (Set)


type Instruction
    = Acc Int
    | Jmp Int
    | Nop Int


type alias State =
    { index : Int
    , acc : Int
    , executed : Set Int
    , done : Bool
    , accOrJmpIndexToChange : Int
    , accOrJmpIndex : Int
    }


toInstruction : String -> Instruction
toInstruction string =
    case String.split " " string of
        "acc" :: value :: [] ->
            Acc (Maybe.withDefault 0 (String.toInt value))

        "jmp" :: value :: [] ->
            Jmp (Maybe.withDefault 0 (String.toInt value))

        "nop" :: value :: [] ->
            Nop (Maybe.withDefault 0 (String.toInt value))

        _ ->
            Nop 0


runProgram : State -> Array Instruction -> State
runProgram startState instructions =
    Array.foldl
        (\_ state ->
            let
                jmp val =
                    { state
                        | index = state.index + val
                        , executed = Set.insert state.index state.executed
                        , accOrJmpIndex = state.accOrJmpIndex + 1
                    }

                nop =
                    { state
                        | executed = Set.insert state.index state.executed
                        , index = state.index + 1
                    }
            in
            if Set.member state.index state.executed || state.index >= Array.length instructions || state.done then
                { state | done = True }

            else
                case
                    ( state.accOrJmpIndex == state.accOrJmpIndexToChange
                    , Array.get state.index instructions |> Maybe.withDefault (Nop 0)
                    )
                of
                    ( _, Acc val ) ->
                        { state
                            | acc = state.acc + val
                            , executed = Set.insert state.index state.executed
                            , index = state.index + 1
                            , accOrJmpIndex = state.accOrJmpIndex + 1
                        }

                    ( False, Jmp val ) ->
                        jmp val

                    ( False, Nop _ ) ->
                        nop

                    ( True, Jmp _ ) ->
                        nop

                    ( True, Nop val ) ->
                        jmp val
        )
        startState
        instructions


test =
    """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""


solve1 : String
solve1 =
    Day8.Input.input
        |> String.split "\n"
        |> List.map toInstruction
        |> Array.fromList
        |> runProgram
            { index = 0
            , acc = 0
            , executed = Set.empty
            , done = False
            , accOrJmpIndexToChange = -1
            , accOrJmpIndex = 0
            }
        |> .acc
        |> String.fromInt


solve2 : String
solve2 =
    let
        instructions =
            Day8.Input.input
                |> String.split "\n"
                |> List.map toInstruction
                |> Array.fromList

        instructionToChange =
            List.Extra.find
                (\i ->
                    let
                        res : State
                        res =
                            runProgram
                                { index = 0
                                , acc = 0
                                , executed = Set.empty
                                , done = False
                                , accOrJmpIndexToChange = i
                                , accOrJmpIndex = 0
                                }
                                instructions
                    in
                    res.index >= Array.length instructions
                )
                (List.range 0 (Array.length instructions))
    in
    case instructionToChange of
        Just i ->
            runProgram
                { index = 0
                , acc = 0
                , executed = Set.empty
                , done = False
                , accOrJmpIndexToChange = i
                , accOrJmpIndex = 0
                }
                instructions
                |> .acc
                |> String.fromInt

        Nothing ->
            "No solution"


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
