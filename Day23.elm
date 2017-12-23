module Day23 exposing (..)

import Parser exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Char
import Arithmetic


type alias State =
    { instructions : Array Instruction
    , index : Int
    , registers : Registers
    , mulCount : Int
    }


type alias Registers =
    Dict String Int


type Instruction
    = Set String Value
    | Sub String Value
    | Mul String Value
    | JNZ Value Value


type Value
    = Register String
    | StaticValue Int


instrParser : Parser Instruction
instrParser =
    Parser.succeed identity
        |= Parser.oneOf [ setParser, subParser, mulParser, jnzParser ]
        |. newlines


newlines =
    Parser.keep zeroOrMore ((==) '\n')


spaces =
    Parser.keep oneOrMore ((==) ' ')


registerParser =
    Parser.keep oneOrMore (Char.isLower)


intParser =
    Parser.oneOf
        [ Parser.int
        , Parser.succeed ((*) -1)
            |. Parser.symbol "-"
            |= Parser.int
        ]


valueParser =
    oneOf
        [ Parser.map StaticValue intParser
        , Parser.map Register registerParser
        ]


setParser =
    Parser.inContext "setParser" <|
        Parser.succeed Set
            |. symbol "set"
            |. spaces
            |= registerParser
            |. spaces
            |= valueParser


subParser =
    Parser.inContext "subParser" <|
        Parser.succeed Sub
            |. symbol "sub"
            |. spaces
            |= registerParser
            |. spaces
            |= valueParser


mulParser =
    Parser.inContext "mulParser" <|
        Parser.succeed Mul
            |. symbol "mul"
            |. spaces
            |= registerParser
            |. spaces
            |= valueParser


jnzParser =
    Parser.inContext "jnzParser" <|
        Parser.succeed JNZ
            |. symbol "jnz"
            |. spaces
            |= valueParser
            |. spaces
            |= valueParser


instructionsParser =
    Parser.succeed identity
        |= Parser.repeat oneOrMore instrParser
        |. Parser.end


getValue : Value -> Registers -> Int
getValue value registers =
    case value of
        Register register ->
            Dict.get register registers |> Maybe.withDefault 0

        StaticValue number ->
            number


initState : String -> State
initState input =
    let
        instructions =
            Parser.run instructionsParser input
                |> Result.mapError (Debug.log "Parse Error")
                |> Result.withDefault []
                |> Array.fromList
    in
        { instructions = instructions
        , index = 0
        , registers = Dict.empty
        , mulCount = 0
        }


initState2 : String -> State
initState2 input =
    let
        instructions =
            Parser.run instructionsParser input
                |> Result.mapError (Debug.log "Parse Error")
                |> Result.withDefault []
                |> Array.fromList
    in
        { instructions = instructions
        , index = 0
        , registers =
            Dict.singleton "a" 1
        , mulCount = 0
        }


input =
    """set b 65
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23
"""


normalPseudo =
    """
b = 65
c = b (65)
if a /= 0
    b *= 100
    b += 100000
    c = b
    c += 17000
LABEL B
f = 1
d = 2
e = 2
label A
//do
g = d (2)
g *= e
g -= b
// g = d * e - b
if g == 0
    f = 0
e += 1
g = e
g -= b
if g /= 0
    GOTO A
d += 1
g = d
g -= b
if g /= 0
    GOTO A
//while e /= b
if f == 0
    h += 1
g = b
g -= c
// g = b - c
if g (b - c) == 0
    TERMINATE
b += 17
GOTO B
"""


pseudo2 =
    """
b = 65
c = b
if(debug) then
    b = b * 100 + 100000
    c = b + 17000
while (b != c)
    toIncrementH = false
    d = 2
    while d != b
        e = 2
        while e != b do
            if d * e == b then
                toIncrementH = true
            e += 1
        d += 1
    if toIncrementH then
        h += 1
    b += 17
"""



-- it is testing all numbers in a range to see whether they are not prime.
-- the range is 106500 to 123500 in steps of 17


h =
    List.range 0 1000 |> List.map ((*) 17) |> List.map ((+) 106500) |> List.filter isNotPrime |> List.length


isNotPrime =
    Arithmetic.isPrime >> not



-- Ths stuff above is for part2.


part1 : String -> Int
part1 input =
    let
        state =
            initState input
    in
        part1Helper state


part1Helper : State -> Int
part1Helper state =
    case Array.get state.index state.instructions of
        Nothing ->
            state.mulCount

        Just instr ->
            part1Helper (executeInstruction instr state)


executeInstruction : Instruction -> State -> State
executeInstruction instr state =
    case instr of
        Set register value ->
            { state
                | registers = Dict.insert register (getValue value state.registers) state.registers
                , index = state.index + 1
            }

        Sub register value ->
            { state
                | registers = Dict.update register (Maybe.withDefault 0 >> (\x -> x - (getValue value state.registers)) >> Just) state.registers
                , index = state.index + 1
            }

        Mul register value ->
            { state
                | registers = Dict.update register (Maybe.withDefault 0 >> (\x -> x * (getValue value state.registers)) >> Just) state.registers
                , index = state.index + 1
                , mulCount = state.mulCount + 1
            }

        JNZ testVal jumpVal ->
            let
                jump =
                    if (getValue testVal state.registers) /= 0 then
                        getValue jumpVal state.registers
                    else
                        1
            in
                { state | index = state.index + jump }



-- This actually runs the stupid algorithm that they have written.
-- Don't use this, it will run for a really really long time.
-- The answer to part 2 is in `h`, above.


part2 : String -> Maybe Int
part2 input =
    let
        state =
            initState2 input
    in
        part2Helper state


part2Helper : State -> Maybe Int
part2Helper state =
    case Array.get state.index state.instructions of
        Nothing ->
            Dict.get "h" state.registers

        Just instr ->
            part2Helper (executeInstruction instr state)
