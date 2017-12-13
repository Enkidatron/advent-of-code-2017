module Day13 exposing (..)


parsePairs =
    String.lines >> List.filterMap (String.split ": " >> List.filterMap (String.toInt >> Result.toMaybe) >> makePair)


makePair list =
    case list of
        a :: b :: [] ->
            Just ( a, b )

        _ ->
            Nothing


part1 =
    parsePairs >> List.filter (areWeCaught 0) >> List.map severity >> List.sum


areWeCaught : Int -> ( Int, Int ) -> Bool
areWeCaught delay ( depth, range ) =
    ((depth + delay) % (range + range - 2)) == 0


severity ( depth, range ) =
    depth * range


part2 =
    parsePairs >> checkForJailbreak 0


checkForJailbreak : Int -> List ( Int, Int ) -> Int
checkForJailbreak delay list =
    case List.filter (areWeCaught delay) list of
        [] ->
            delay

        _ ->
            checkForJailbreak (delay + 1) list


testInput =
    """0: 3
1: 2
4: 4
6: 4"""


input =
    """0: 3
1: 2
2: 4
4: 8
6: 5
8: 6
10: 6
12: 4
14: 6
16: 6
18: 17
20: 8
22: 8
24: 8
26: 9
28: 8
30: 12
32: 12
34: 10
36: 12
38: 12
40: 8
42: 12
44: 12
46: 10
48: 12
50: 12
52: 14
54: 14
56: 12
58: 14
60: 14
62: 14
64: 14
66: 14
68: 12
70: 14
72: 14
74: 14
76: 14
80: 18
82: 14
90: 18"""
