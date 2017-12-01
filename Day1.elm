module Day1 exposing (..)

import List.Extra


toNumberList : String -> List Int
toNumberList =
    String.toList >> List.filterMap (List.singleton >> String.fromList >> String.toInt >> Result.toMaybe)


spinl : Int -> List a -> List a
spinl amount list =
    (List.drop amount list) ++ (List.take amount list)


evalPair : ( Int, Int ) -> Int
evalPair ( x, y ) =
    if x == y then
        x
    else
        0


makePairsPart1 : List a -> List ( a, a )
makePairsPart1 list =
    List.Extra.zip list (spinl 1 list)


makePairsPart2 : List a -> List ( a, a )
makePairsPart2 list =
    List.Extra.zip list (spinl (List.length list // 2) list)


solvePart1 : String -> Int
solvePart1 =
    toNumberList >> makePairsPart1 >> List.map evalPair >> List.sum


solvePart2 : String -> Int
solvePart2 =
    toNumberList >> makePairsPart2 >> List.map evalPair >> List.sum
