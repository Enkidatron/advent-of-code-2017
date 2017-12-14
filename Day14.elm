module Day14 exposing (..)

import Day10
import Graph exposing (..)
import IntDict


input =
    "oundnydw"


testInput =
    "flqrgnkx"


makeStringList : String -> List String
makeStringList word =
    List.map (\number -> word ++ "-" ++ Basics.toString number) (List.range 0 127)


hashString : String -> String
hashString =
    Day10.doPart2


countRow : String -> Int
countRow =
    String.toList >> List.map countHexChar >> List.sum


countHexChar : Char -> Int
countHexChar char =
    case char of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            1

        '3' ->
            2

        '4' ->
            1

        '5' ->
            2

        '6' ->
            2

        '7' ->
            3

        '8' ->
            1

        '9' ->
            2

        'a' ->
            2

        'b' ->
            3

        'c' ->
            2

        'd' ->
            3

        'e' ->
            3

        'f' ->
            4

        _ ->
            0


doPart1 : String -> Int
doPart1 =
    makeStringList >> List.map (hashString >> countRow) >> List.sum


doPart2 : String -> Int
doPart2 =
    makeStringList >> List.map hashString >> makeGraph >> countGroups


nodeIdForCoord ( row, col ) =
    (row * 128) + col


makeGraph hashList =
    let
        makeNodeFromCoord coord =
            Graph.Node (nodeIdForCoord coord) coord

        makeNodesForRow rowIndex hash =
            let
                activeCoordinates colIndex char =
                    case char of
                        '0' ->
                            []

                        '1' ->
                            [ ( rowIndex, (colIndex * 4) + 3 ) ]

                        '2' ->
                            [ ( rowIndex, (colIndex * 4) + 2 ) ]

                        '3' ->
                            [ ( rowIndex, (colIndex * 4) + 2 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        '4' ->
                            [ ( rowIndex, (colIndex * 4) + 1 ) ]

                        '5' ->
                            [ ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        '6' ->
                            [ ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 2 ) ]

                        '7' ->
                            [ ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 2 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        '8' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ) ]

                        '9' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        'a' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 2 ) ]

                        'b' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 2 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        'c' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 1 ) ]

                        'd' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        'e' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 2 ) ]

                        'f' ->
                            [ ( rowIndex, (colIndex * 4) + 0 ), ( rowIndex, (colIndex * 4) + 1 ), ( rowIndex, (colIndex * 4) + 2 ), ( rowIndex, (colIndex * 4) + 3 ) ]

                        _ ->
                            []
            in
                String.toList hash |> List.indexedMap activeCoordinates |> List.concat |> List.map makeNodeFromCoord

        nodes =
            List.indexedMap makeNodesForRow hashList |> List.concat

        inBounds ( row, col ) =
            row >= 0 && row <= 127 && col >= 0 && col <= 127

        addEdges graph context =
            let
                ( row, col ) =
                    context.node.label

                edges =
                    [ ( row - 1, col )
                    , ( row + 1, col )
                    , ( row, col - 1 )
                    , ( row, col + 1 )
                    ]
                        |> List.filter inBounds
                        |> List.filterMap (\coord -> Graph.get (nodeIdForCoord coord) graph)
                        |> List.map (.node >> .id >> flip (,) ())
                        |> IntDict.fromList
            in
                { context | outgoing = edges, incoming = edges }
    in
        Graph.fromNodesAndEdges nodes [] |> (\graph -> Graph.mapContexts (addEdges graph) graph)


getSubGraphs graph =
    case Graph.stronglyConnectedComponents graph of
        Ok _ ->
            [ graph ]

        Err subgraphs ->
            subgraphs


countGroups =
    getSubGraphs >> List.length


getSubGraphWithCoord coord subgraphs =
    let
        coordInGraph graph =
            case Graph.get (nodeIdForCoord coord) graph of
                Just _ ->
                    True

                Nothing ->
                    False
    in
        List.filter coordInGraph subgraphs |> List.head |> Maybe.withDefault Graph.empty
