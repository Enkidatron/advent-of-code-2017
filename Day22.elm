module Day22 exposing (..)

import Matrix
import Dict exposing (..)


input =
    """....##.#.#.#...#.##.##.#.
##.####..###..#.#.#.###.#
.#.#...#.##....#......###
...#.....##.###....##.###
#.########.#.#####..##.#.
.#..#..#.#..#....##.#...#
.....#.##..#.#.....##..##
....###....###....###.#..
..#..#..#..#.##.#.#..##.#
.##......#...##.#.#.##.#.
.#####.#.#.##...###...#..
#..###..#....#....##..#..
###..#....#.##.##.....#..
##.##..#.##.#..#####.#.#.
#....#.######.#.#.#.##.#.
###.##.#.######.#..###.#.
#...###.#.#..##..####....
###...##.###..###..##..#.
..##.###...#.....##.##.##
..##..#.###.###.....#.###
#..###.##.#.###......####
#.#...#..##.###.....##.#.
#..#.##...##.##....#...#.
..#.#..#..#...##.#..###..
......###....#.....#....#"""


testInput =
    """..#
#..
..."""


type alias Map =
    Dict Position Bool


type alias Position =
    ( Int, Int )


parseLinesToMap : String -> Map
parseLinesToMap =
    String.lines
        >> List.map (String.split "")
        >> Matrix.fromList
        >> Matrix.map ((==) "#")
        >> (\matrix ->
                let
                    rowAdjust =
                        Matrix.rowCount matrix // 2

                    colAdjust =
                        Matrix.colCount matrix // 2
                in
                    Matrix.mapWithLocation (\( row, col ) value -> ( ( col - colAdjust, row - rowAdjust ), value )) matrix
           )
        >> Matrix.flatten
        >> Dict.fromList


type alias State =
    { pos : Position
    , dir : Direction
    , infections : Int
    , map : Map
    }


type Direction
    = Up
    | Down
    | Right
    | Left


turnLeft : Direction -> Direction
turnLeft dir =
    case dir of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


initState map =
    { pos = ( 0, 0 )
    , dir = Up
    , infections = 0
    , map = map
    }


part1 : String -> Int
part1 input =
    let
        map =
            parseLinesToMap input

        startState =
            initState map
    in
        part1Helper 10000 startState


part1Helper : Int -> State -> Int
part1Helper stepsLeft state =
    if stepsLeft <= 0 then
        state.infections
    else
        part1Helper (stepsLeft - 1) (stepVirus state)


isInfected : Position -> Map -> Bool
isInfected pos map =
    Dict.get pos map |> Maybe.withDefault False


walkForward : Direction -> Position -> Position
walkForward dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


stepVirus : State -> State
stepVirus state =
    let
        ( turn, newInfections ) =
            if isInfected state.pos state.map then
                ( turnRight, state.infections )
            else
                ( turnLeft, state.infections + 1 )

        newDir =
            turn state.dir

        newPos =
            walkForward newDir state.pos

        newMap =
            Dict.insert state.pos (not <| isInfected state.pos state.map) state.map
    in
        { pos = newPos
        , dir = newDir
        , infections = newInfections
        , map = newMap
        }


type alias Map2 =
    Dict Position Node


type Node
    = Clean
    | Weakened
    | Infected
    | Flagged


type alias State2 =
    { pos : Position
    , dir : Direction
    , infections : Int
    , map : Map2
    }


upgradeMap : Map -> Map2
upgradeMap =
    Dict.map
        (\pos infected ->
            if infected then
                Infected
            else
                Clean
        )


cycleNode : Node -> Node
cycleNode node =
    case node of
        Clean ->
            Weakened

        Weakened ->
            Infected

        Infected ->
            Flagged

        Flagged ->
            Clean


part2 : String -> Int
part2 input =
    let
        map =
            parseLinesToMap input |> upgradeMap

        state =
            initState map
    in
        part2Helper 10000000 state


part2Helper : Int -> State2 -> Int
part2Helper stepsLeft state =
    if stepsLeft <= 0 then
        state.infections
    else
        part2Helper (stepsLeft - 1) (stepVirus2 state)


stepVirus2 : State2 -> State2
stepVirus2 state =
    let
        currentNode =
            Dict.get state.pos state.map |> Maybe.withDefault Clean

        newDir =
            case currentNode of
                Clean ->
                    turnLeft state.dir

                Weakened ->
                    state.dir

                Infected ->
                    turnRight state.dir

                Flagged ->
                    turnRight <| turnRight state.dir

        newPos =
            walkForward newDir state.pos

        newInfections =
            if currentNode == Weakened then
                state.infections + 1
            else
                state.infections

        newMap =
            Dict.insert state.pos (cycleNode currentNode) state.map
    in
        { pos = newPos
        , dir = newDir
        , infections = newInfections
        , map = newMap
        }
