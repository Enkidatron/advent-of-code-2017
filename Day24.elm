module Day24 exposing (..)

import Parser exposing (..)
import List.Extra


type alias Component =
    ( Int, Int )


type alias Bridge =
    { strength : Int
    , length : Int
    , next : Int
    }


newlines =
    Parser.keep zeroOrMore ((==) '\n')


componentParser =
    Parser.succeed (,)
        |= Parser.int
        |. Parser.symbol "/"
        |= Parser.int
        |. newlines


componentsParser =
    Parser.repeat oneOrMore componentParser
        |. Parser.end


startBridge : Bridge
startBridge =
    { strength = 0, next = 0, length = 0 }


part1 : String -> Int
part1 input =
    let
        components =
            Parser.run componentsParser input |> Result.mapError (Debug.log "Parse Error") |> Result.withDefault []
    in
        buildAllBridges startBridge components
            |> List.map .strength
            |> List.sort
            |> List.reverse
            |> List.head
            |> Maybe.withDefault 0


part2 : String -> Int
part2 input =
    let
        components =
            Parser.run componentsParser input |> Result.mapError (Debug.log "Parse Error") |> Result.withDefault []
    in
        buildAllBridges startBridge components
            |> List.sortWith part2compare
            |> List.head
            |> Maybe.map .strength
            |> Maybe.withDefault 0


part2compare : Bridge -> Bridge -> Order
part2compare bridge1 bridge2 =
    case compare bridge1.length bridge2.length of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            case compare bridge1.strength bridge2.strength of
                LT ->
                    GT

                GT ->
                    LT

                EQ ->
                    EQ


buildAllBridges : Bridge -> List Component -> List Bridge
buildAllBridges bridge components =
    let
        validNextComponents =
            List.filter (\( comp1, comp2 ) -> comp1 == bridge.next || comp2 == bridge.next) components
    in
        case validNextComponents of
            [] ->
                [ bridge ]

            _ ->
                List.concatMap
                    (\(( comp1, comp2 ) as component) ->
                        buildAllBridges
                            { strength = bridge.strength + comp1 + comp2
                            , next = getOtherNumber bridge.next component
                            , length = bridge.length + 1
                            }
                            (List.Extra.remove component components)
                    )
                    validNextComponents


getOtherNumber : Int -> Component -> Int
getOtherNumber num ( comp1, comp2 ) =
    if num == comp1 then
        comp2
    else
        comp1


input =
    """50/41
19/43
17/50
32/32
22/44
9/39
49/49
50/39
49/10
37/28
33/44
14/14
14/40
8/40
10/25
38/26
23/6
4/16
49/25
6/39
0/50
19/36
37/37
42/26
17/0
24/4
0/36
6/9
41/3
13/3
49/21
19/34
16/46
22/33
11/6
22/26
16/40
27/21
31/46
13/2
24/7
37/45
49/2
32/11
3/10
32/49
36/21
47/47
43/43
27/19
14/22
13/43
29/0
33/36
2/6
"""
