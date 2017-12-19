module Day19 exposing (..)

import Matrix exposing (..)
import List.Extra
import Stream exposing (..)


type Terrain
    = Path
    | NamedPath String
    | Wilderness


type alias Position =
    ( Int, Int )


isPath : Terrain -> Bool
isPath =
    ((/=) Wilderness)


type alias Map =
    Matrix Terrain


stringToMap : String -> Map
stringToMap =
    String.lines >> List.map (String.split "" >> List.map stringToTerrain) >> Matrix.fromList


stringToTerrain : String -> Terrain
stringToTerrain word =
    case word of
        "-" ->
            Path

        "+" ->
            Path

        "|" ->
            Path

        " " ->
            Wilderness

        _ ->
            NamedPath word


type Facing
    = Up
    | Down
    | Right
    | Left


type alias State =
    { position : Position
    , facing : Facing
    , encountered : List String
    , stepsTaken : Int
    }


getStartingPositionForMap : Map -> Position
getStartingPositionForMap map =
    Matrix.toList map
        |> List.head
        |> Maybe.withDefault []
        |> List.indexedMap (,)
        |> List.filter (Tuple.second >> isPath)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1
        |> (,) -1


initState : String -> State
initState input =
    let
        map =
            stringToMap input

        startPos =
            getStartingPositionForMap map
    in
        { position = startPos
        , facing = Down
        , encountered = []
        , stepsTaken = 0
        }


movePositionInDirection : Position -> Facing -> Position
movePositionInDirection ( row, col ) facing =
    case facing of
        Up ->
            ( row - 1, col )

        Down ->
            ( row + 1, col )

        Right ->
            ( row, col + 1 )

        Left ->
            ( row, col - 1 )


getNeighborsExceptBehind : Position -> Facing -> List ( Position, Facing )
getNeighborsExceptBehind position facing =
    [ Up, Down, Right, Left ]
        |> List.Extra.remove (oppositeFacing facing)
        |> List.map (\facing -> ( movePositionInDirection position facing, facing ))


oppositeFacing : Facing -> Facing
oppositeFacing facing =
    case facing of
        Up ->
            Down

        Down ->
            Up

        Right ->
            Left

        Left ->
            Right


getName : Terrain -> Maybe String
getName terrain =
    case terrain of
        NamedPath word ->
            Just word

        _ ->
            Nothing


takeStep : Map -> State -> State
takeStep map state =
    let
        forward =
            movePositionInDirection state.position state.facing

        goAnywhere =
            getNeighborsExceptBehind state.position state.facing
                |> List.filterMap (\( pos, facing ) -> Matrix.get pos map |> Maybe.map ((,) ( pos, facing )))
                |> List.filter (Tuple.second >> isPath)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault ( state.position, state.facing )

        ( nextPosition, newFacing ) =
            case Matrix.get forward map of
                Just terrain ->
                    if isPath terrain then
                        ( forward, state.facing )
                    else
                        goAnywhere

                Nothing ->
                    goAnywhere

        maybeNamed =
            Matrix.get nextPosition map
                |> Maybe.andThen getName

        encountered =
            case maybeNamed of
                Just newName ->
                    state.encountered ++ [ newName ]

                Nothing ->
                    state.encountered
    in
        { state
            | position = nextPosition
            , facing = newFacing
            , encountered = encountered
            , stepsTaken = state.stepsTaken + 1
        }


weAreAtTheEnd : Map -> State -> Bool
weAreAtTheEnd map state =
    getNeighborsExceptBehind state.position state.facing
        |> List.filterMap (Tuple.first >> flip Matrix.get map)
        |> List.filter isPath
        |> List.length
        |> (\length -> length == 0)


part1 : String -> Maybe String
part1 input =
    let
        initialState =
            initState input

        map =
            stringToMap input
    in
        Stream.iterate initialState (takeStep map)
            |> Stream.next
            |> Tuple.first
            |> Stream.next
            |> Tuple.first
            |> Stream.dropWhile (not << (weAreAtTheEnd map))
            |> Stream.next
            |> Tuple.second
            |> Maybe.map (.encountered >> String.join "")


part2 : String -> Int
part2 input =
    let
        initialState =
            initState input

        map =
            stringToMap input
    in
        Stream.iterate initialState (takeStep map)
            |> Stream.next
            |> Tuple.first
            |> Stream.next
            |> Tuple.first
            |> Stream.dropWhile (not << (weAreAtTheEnd map))
            |> Stream.next
            |> Tuple.second
            |> Maybe.map .stepsTaken
            |> Maybe.withDefault 0
