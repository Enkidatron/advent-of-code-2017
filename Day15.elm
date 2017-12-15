module Day15 exposing (..)

import Bitwise exposing (..)
import List.Extra


genAfactor =
    16807


genBfactor =
    48271


denom =
    2147483647


testA =
    65


testB =
    8921


inputA =
    277


inputB =
    349


step factor last =
    Basics.rem (last * factor) denom


stepA =
    step genAfactor


stepB =
    step genBfactor


stepPair ( lastA, lastB ) =
    ( stepA lastA, stepB lastB )


comparePair ( a, b ) =
    (Bitwise.and a 65535) == (Bitwise.and b 65535)


countMatches : Int -> Int -> ( Int, Int ) -> Int
countMatches matches roundsLeft pair =
    let
        newPair =
            stepPair pair
    in
        if roundsLeft == 0 then
            matches
        else if (comparePair newPair) then
            countMatches (matches + 1) (roundsLeft - 1) newPair
        else
            countMatches (matches) (roundsLeft - 1) newPair


getNextA : Int -> Int
getNextA lastVal =
    let
        tryNext =
            stepA lastVal
    in
        if Basics.rem tryNext 4 == 0 then
            tryNext
        else
            getNextA tryNext


getNextB : Int -> Int
getNextB lastVal =
    let
        tryNext =
            stepB lastVal
    in
        if Basics.rem tryNext 8 == 0 then
            tryNext
        else
            getNextB tryNext



--makeListXLong : Int -> (a -> a) -> a -> List a -> List a
--makeListXLong length generator default listSoFar =
--    if List.length listSoFar >= length then
--        listSoFar
--    else
--        makeListXLong length generator default <|
--            (List.head listSoFar |> Maybe.withDefault default |> generator)
--                :: listSoFar
--part2listA whichInput =
--    makeListXLong 5000000 getNextA whichInput []
--part2listB whichInput =
--    makeListXLong 5000000 getNextB whichInput []
--part2 ( whichA, whichB ) =
--    -- This seems to hang, actually. Use countMatches2 instead.
--    List.Extra.zip (part2listA whichA) (part2listB whichB) |> List.filter comparePair |> List.length


stepPair2 ( a, b ) =
    ( getNextA a, getNextB b )


countMatches2 : Int -> Int -> ( Int, Int ) -> Int
countMatches2 matches roundsLeft pair =
    let
        newPair =
            stepPair2 pair
    in
        if roundsLeft == 0 then
            matches
        else if (comparePair newPair) then
            countMatches2 (matches + 1) (roundsLeft - 1) newPair
        else
            countMatches2 (matches) (roundsLeft - 1) newPair
