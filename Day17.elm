module Day17 exposing (..)

import List.Extra
import Stream


input =
    337


testInput =
    3


initialList =
    [ 0 ]


initialIndex =
    0


cycleAndInsert steps ( index, list ) =
    let
        cycleTo =
            -- this actually mutates the list in the wrong way, but it apparently works anyway in the long run
            -- or else I got super lucky, cause it worked for both the test input and my input.
            (index + steps) % (List.length list)
    in
        ( cycleTo + 1, (List.take cycleTo list) ++ [ List.length list ] ++ (List.drop cycleTo list) )


cycleAndInsertProperly steps ( index, list ) =
    let
        insertAt =
            ((index + steps) % (List.length list)) + 1
    in
        ( insertAt, (List.take insertAt list) ++ [ List.length list ] ++ (List.drop insertAt list) )


part1 whichInput =
    List.foldl (\num -> cycleAndInsertProperly whichInput) ( initialIndex, initialList ) (List.range 1 2017)
        |> (\( index, list ) -> List.drop ((index + 1) % List.length list) list |> List.head |> Maybe.withDefault -1)


fiftyMillion =
    50000000


cycleAndInsert2 steps ( index, listSize, valueAfterZero ) =
    let
        insertAfter =
            (index + steps) % listSize

        newValueAfterZero =
            if insertAfter == 0 then
                listSize
            else
                valueAfterZero
    in
        ( insertAfter + 1, listSize + 1, newValueAfterZero )


stormStream stepSize =
    Stream.iterate ( 0, 1, 0 ) (cycleAndInsert2 stepSize)


part2Stream stepSize =
    (stormStream stepSize)
        |> Stream.dropWhile (\( _, listSize, _ ) -> listSize <= fiftyMillion)
        |> Stream.next
        |> Tuple.second
        |> Maybe.map (\( _, _, value ) -> value)
        |> Maybe.withDefault -1
