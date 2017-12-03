module Day3 exposing (..)

import Dict exposing (Dict)


type alias Position =
    ( Int, Int )


distance : Position -> Int
distance ( x, y ) =
    Basics.abs x + Basics.abs y


getPosition : Int -> Position
getPosition steps =
    getPositionRecursively steps Right 1 1 ( 0, 0 )


type Direction
    = Right
    | Left
    | Up
    | Down


getPositionRecursively : Int -> Direction -> Int -> Int -> Position -> Position
getPositionRecursively stepsLeft facing stepsInSegment segmentStepsRemaining ( x, y ) =
    case ( stepsLeft, facing, segmentStepsRemaining ) of
        ( 1, _, _ ) ->
            ( x, y )

        ( _, Right, 1 ) ->
            getPositionRecursively (stepsLeft - 1) Up stepsInSegment stepsInSegment ( x + 1, y )

        ( _, Right, _ ) ->
            getPositionRecursively (stepsLeft - 1) Right stepsInSegment (segmentStepsRemaining - 1) ( x + 1, y )

        ( _, Up, 1 ) ->
            getPositionRecursively (stepsLeft - 1) Left (stepsInSegment + 1) (stepsInSegment + 1) ( x, y + 1 )

        ( _, Up, _ ) ->
            getPositionRecursively (stepsLeft - 1) Up stepsInSegment (segmentStepsRemaining - 1) ( x, y + 1 )

        ( _, Left, 1 ) ->
            getPositionRecursively (stepsLeft - 1) Down stepsInSegment stepsInSegment ( x - 1, y )

        ( _, Left, _ ) ->
            getPositionRecursively (stepsLeft - 1) Left stepsInSegment (segmentStepsRemaining - 1) ( x - 1, y )

        ( _, Down, 1 ) ->
            getPositionRecursively (stepsLeft - 1) Right (stepsInSegment + 1) (stepsInSegment + 1) ( x, y - 1 )

        ( _, Down, _ ) ->
            getPositionRecursively (stepsLeft - 1) Down stepsInSegment (segmentStepsRemaining - 1) ( x, y - 1 )


solvePart1 : Int -> Int
solvePart1 =
    getPosition >> distance


solvePart2 : Int -> Int
solvePart2 targetValue =
    solvePart2Recursively targetValue (Dict.singleton ( 0, 0 ) 1) 1


solvePart2Recursively : Int -> Dict Position Int -> Int -> Int
solvePart2Recursively targetValue memoryDict memoryCell =
    let
        newPosition =
            getPosition (memoryCell + 1)

        valueForNextMemoryCell =
            getNeighborValuesSum newPosition memoryDict
    in
        if valueForNextMemoryCell > targetValue then
            valueForNextMemoryCell
        else
            solvePart2Recursively targetValue (Dict.insert newPosition valueForNextMemoryCell memoryDict) (memoryCell + 1)


getNeighborValuesSum : Position -> Dict Position Int -> Int
getNeighborValuesSum ( x, y ) memoryCells =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.filterMap (\pos -> Dict.get pos memoryCells)
        |> List.sum
