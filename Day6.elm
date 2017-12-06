module Day6 exposing (..)

import Array exposing (Array)
import Array.Extra
import Set exposing (Set)
import Dict exposing (Dict)


inputList =
    [ 11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11 ]


step : Int -> Set (List Int) -> Array Int -> Int
step stepsTaken arraysSeen array =
    let
        newArray =
            mutate array
    in
        case Set.member (Array.toList newArray) arraysSeen of
            True ->
                (stepsTaken + 1)

            False ->
                step (stepsTaken + 1) (Set.insert (Array.toList newArray) arraysSeen) newArray


mutate : Array Int -> Array Int
mutate array =
    let
        customCompare ( indexA, sizeA ) ( indexB, sizeB ) =
            case compare sizeA sizeB of
                LT ->
                    GT

                GT ->
                    LT

                EQ ->
                    compare indexA indexB

        cellToRedistribute =
            array
                |> Array.toIndexedList
                |> List.sortWith customCompare
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
    in
        case Array.get cellToRedistribute array of
            Nothing ->
                array

            Just amountToRedistribute ->
                distributePart1 amountToRedistribute (Basics.rem (cellToRedistribute + 1) (Array.length array)) (Array.set cellToRedistribute 0 array)


distributePart1 : Int -> Int -> Array Int -> Array Int
distributePart1 amountToRedistribute nextCell array =
    --let
    --_ =
    --    Debug.log ("Redistributing " ++ toString amountToRedistribute ++ " starting at " ++ toString nextCell) array
    --in
    case amountToRedistribute of
        0 ->
            array

        _ ->
            distributePart1 (amountToRedistribute - 1) (Basics.rem (nextCell + 1) (Array.length array)) (Array.Extra.update nextCell ((+) 1) array)


processPart1 : List Int -> Int
processPart1 list =
    step 0 (Set.singleton list) (Array.fromList list)


step2 : Int -> Dict (List Int) Int -> Array Int -> Int
step2 stepsTaken arraysSeen array =
    let
        newArray =
            mutate array
    in
        case Dict.get (Array.toList newArray) arraysSeen of
            Just loopStart ->
                stepsTaken - loopStart

            Nothing ->
                step2 (stepsTaken + 1) (Dict.insert (Array.toList newArray) stepsTaken arraysSeen) newArray


processPart2 : List Int -> Int
processPart2 list =
    step2 0 (Dict.singleton list 0) (Array.fromList list)
