module Day10 exposing (..)

import List.Extra
import Bitwise
import String
import Char


input =
    [ 183, 0, 31, 146, 254, 240, 223, 150, 2, 206, 161, 1, 255, 232, 199, 88 ]


inputString =
    "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88"


knotList =
    List.range 0 255


type alias State =
    { index : Int
    , stepSize : Int
    , knotList : List Int
    }


solvePart1 : List Int -> List Int -> Int
solvePart1 knotList input =
    List.foldl twist1 (State 0 0 knotList) input |> .knotList |> sumFirstTwo


sumFirstTwo list =
    case list of
        a :: b :: _ ->
            a * b

        _ ->
            0


twist1 : Int -> State -> State
twist1 length state =
    let
        overflow =
            Basics.max (state.index + length - (List.length state.knotList)) 0

        sublist =
            (state.knotList |> List.drop state.index |> List.take length)
                ++ (state.knotList |> List.take overflow)

        reversedSublist =
            List.reverse sublist

        beginningSwapped =
            List.drop (List.length sublist - overflow) reversedSublist

        endSwapped =
            List.take (List.length sublist - overflow) reversedSublist

        untouchedBeforeIndex =
            state.knotList
                |> List.drop overflow
                |> List.take (state.index - overflow)

        untouchedAfterIndex =
            state.knotList
                |> List.drop (state.index + length)

        newList =
            beginningSwapped ++ untouchedBeforeIndex ++ endSwapped ++ untouchedAfterIndex
    in
        { index = (state.index + length + state.stepSize) % (List.length state.knotList)
        , stepSize = state.stepSize + 1
        , knotList = newList
        }


part1 =
    solvePart1 knotList input


toAsciiList : String -> List Int
toAsciiList =
    String.toList >> List.map Char.toCode >> flip (++) [ 17, 31, 73, 47, 23 ]


doRound : List Int -> State -> State
doRound lengths state =
    List.foldl twist1 state lengths


doXRounds : Int -> List Int -> State -> State
doXRounds x lengths initialState =
    List.foldl (\_ state -> doRound lengths state) initialState (List.range 1 x)


sparseToDenseHash : List Int -> List Int
sparseToDenseHash sparse =
    List.Extra.groupsOf 16 sparse |> List.map (\group -> List.foldl Bitwise.xor 0 group)


doPart2 : String -> String
doPart2 =
    toAsciiList
        >> (\lengths -> doXRounds 64 lengths (State 0 0 knotList))
        >> .knotList
        >> sparseToDenseHash
        >> List.map toHexString
        >> String.join ""


toHexString number =
    let
        toHexDigit smallNumber =
            case smallNumber of
                10 ->
                    "a"

                11 ->
                    "b"

                12 ->
                    "c"

                13 ->
                    "d"

                14 ->
                    "e"

                15 ->
                    "f"

                _ ->
                    toString smallNumber
    in
        (toHexDigit (number // 16)) ++ (toHexDigit (number % 16))
