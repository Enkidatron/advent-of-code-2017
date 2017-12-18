module Day18 exposing (..)

import Array exposing (..)
import Stream exposing (..)
import Parser exposing (..)
import Char
import Dict exposing (Dict)


type alias State =
    { instructions : Array Instruction
    , index : Int
    , lastSound : Int
    , registers : Registers
    }


type alias Registers =
    Dict String Int


type Instruction
    = Send Value
    | Set String Value
    | Add String Value
    | Mul String Value
    | Mod String Value
    | Rcv String
    | JGZ Value Value


type Value
    = Register String
    | StaticValue Int


instrParser : Parser Instruction
instrParser =
    Parser.succeed identity
        |= Parser.oneOf [ sendParser, setParser, addParser, mulParser, modParser, rcvParser, jgzParser ]
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


sendParser =
    Parser.inContext "sendParser" <|
        Parser.succeed Send
            |. symbol "snd"
            |. spaces
            |= valueParser


setParser =
    Parser.inContext "setParser" <|
        Parser.succeed Set
            |. symbol "set"
            |. spaces
            |= registerParser
            |. spaces
            |= valueParser


addParser =
    Parser.inContext "addParser" <|
        Parser.succeed Add
            |. symbol "add"
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


modParser =
    Parser.inContext "modParser" <|
        Parser.succeed Mod
            |. symbol "mod"
            |. spaces
            |= registerParser
            |. spaces
            |= valueParser


rcvParser =
    Parser.inContext "rcvParser" <|
        Parser.succeed Rcv
            |. symbol "rcv"
            |. spaces
            |= registerParser


jgzParser =
    Parser.inContext "jgzParser" <|
        Parser.succeed JGZ
            |. symbol "jgz"
            |. spaces
            |= valueParser
            |. spaces
            |= valueParser


instructionsParser =
    Parser.succeed identity
        |= Parser.repeat oneOrMore instrParser
        |. Parser.end


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
        , lastSound = 0
        , registers = Dict.empty
        }


executeInstruction : State -> State
executeInstruction state =
    let
        instruction =
            Array.get state.index state.instructions
    in
        case instruction of
            Just (Send value) ->
                { state | lastSound = getValue value state.registers, index = state.index + 1 }

            Just (Set target value) ->
                { state
                    | registers = Dict.insert target (getValue value state.registers) state.registers
                    , index = state.index + 1
                }

            Just (Add target value) ->
                { state
                    | registers = Dict.update target (Maybe.withDefault 0 >> (+) (getValue value state.registers) >> Just) state.registers
                    , index = state.index + 1
                }

            Just (Mul target value) ->
                { state
                    | registers = Dict.update target (Maybe.withDefault 0 >> (*) (getValue value state.registers) >> Just) state.registers
                    , index = state.index + 1
                }

            Just (Mod target value) ->
                { state
                    | registers = Dict.update target (Maybe.withDefault 0 >> (flip (%) (getValue value state.registers)) >> Just) state.registers
                    , index = state.index + 1
                }

            Just (Rcv _) ->
                { state | index = state.index + 1 }

            Just (JGZ testVal jumpVal) ->
                let
                    jump =
                        if (getValue testVal state.registers) > 0 then
                            getValue jumpVal state.registers
                        else
                            1
                in
                    { state | index = state.index + jump }

            Nothing ->
                state


getValue : Value -> Registers -> Int
getValue value registers =
    case value of
        Register register ->
            Dict.get register registers |> Maybe.withDefault 0

        StaticValue number ->
            number


duetStream initialState =
    Stream.iterate initialState executeInstruction


weRecoveredSound : State -> Bool
weRecoveredSound state =
    case (Array.get state.index state.instructions) of
        Just (Rcv register) ->
            (Dict.get register state.registers |> Maybe.withDefault 0) > 0

        _ ->
            False


weTerminated : State -> Bool
weTerminated state =
    case (Array.get state.index state.instructions) of
        Nothing ->
            True

        _ ->
            False


part1 : String -> Maybe Int
part1 =
    initState
        >> duetStream
        >> Stream.dropWhile (\state -> not (weRecoveredSound state || weTerminated state))
        >> Stream.next
        >> Tuple.second
        >> Maybe.map .lastSound


type alias State2 =
    { instructions : Array Instruction
    , program0 : ProgramState
    , program1 : ProgramState
    , numberOfValuesSentByProgram1 : Int
    }


type alias ProgramState =
    { index : Int
    , inbox : List Int
    , registers : Registers
    }


initProgram : Int -> ProgramState
initProgram id =
    { index = 0
    , inbox = []
    , registers = Dict.singleton "p" id
    }


initState2 : String -> State2
initState2 input =
    let
        instructions =
            Parser.run instructionsParser input
                |> Result.mapError (Debug.log "Parse Error")
                |> Result.withDefault []
                |> Array.fromList
    in
        { instructions = instructions
        , program0 = initProgram 0
        , program1 = initProgram 1
        , numberOfValuesSentByProgram1 = 0
        }


stepProgram : Array Instruction -> ProgramState -> ( ProgramState, Maybe Int )
stepProgram instructions state =
    case Array.get state.index instructions of
        Just (Send value) ->
            ( { state | index = state.index + 1 }, Just (getValue value state.registers) )

        Just (Set target value) ->
            ( { state
                | registers = Dict.insert target (getValue value state.registers) state.registers
                , index = state.index + 1
              }
            , Nothing
            )

        Just (Add target value) ->
            ( { state
                | registers = Dict.update target (Maybe.withDefault 0 >> (+) (getValue value state.registers) >> Just) state.registers
                , index = state.index + 1
              }
            , Nothing
            )

        Just (Mul target value) ->
            ( { state
                | registers = Dict.update target (Maybe.withDefault 0 >> (*) (getValue value state.registers) >> Just) state.registers
                , index = state.index + 1
              }
            , Nothing
            )

        Just (Mod target value) ->
            ( { state
                | registers = Dict.update target (Maybe.withDefault 0 >> (flip (%) (getValue value state.registers)) >> Just) state.registers
                , index = state.index + 1
              }
            , Nothing
            )

        Just (Rcv register) ->
            case state.inbox of
                [] ->
                    ( state, Nothing )

                message :: tail ->
                    ( { state | registers = Dict.insert register message state.registers, index = state.index + 1, inbox = tail }, Nothing )

        Just (JGZ testVal jumpVal) ->
            let
                jump =
                    if (getValue testVal state.registers) > 0 then
                        getValue jumpVal state.registers
                    else
                        1
            in
                ( { state | index = state.index + jump }, Nothing )

        Nothing ->
            ( state, Nothing )


addMessageToProgramQueue : Int -> ProgramState -> ProgramState
addMessageToProgramQueue message state =
    { state | inbox = state.inbox ++ [ message ] }


executeInstruction2 : State2 -> State2
executeInstruction2 state =
    let
        ( stepped0, out0 ) =
            stepProgram state.instructions state.program0

        ( stepped1, out1 ) =
            stepProgram state.instructions state.program1

        ( new0, newTotalMessages ) =
            case out1 of
                Just message ->
                    ( addMessageToProgramQueue message stepped0, state.numberOfValuesSentByProgram1 + 1 )

                Nothing ->
                    ( stepped0, state.numberOfValuesSentByProgram1 )

        new1 =
            case out0 of
                Just message ->
                    addMessageToProgramQueue message stepped1

                Nothing ->
                    stepped1
    in
        { state
            | program0 = new0
            , program1 = new1
            , numberOfValuesSentByProgram1 = newTotalMessages
        }


weTerminated2 : State2 -> Bool
weTerminated2 state =
    programTerminated state.instructions state.program0 && programTerminated state.instructions state.program1


programTerminated : Array Instruction -> ProgramState -> Bool
programTerminated instructions state =
    case Array.get state.index instructions of
        Nothing ->
            True

        Just (Rcv _) ->
            state.inbox == []

        _ ->
            False


duetStream2 : State2 -> Stream State2
duetStream2 state =
    Stream.iterate state executeInstruction2


part2 : String -> Maybe Int
part2 =
    initState2
        >> duetStream2
        >> Stream.dropWhile (not << weTerminated2)
        >> Stream.next
        >> Tuple.second
        >> Maybe.map .numberOfValuesSentByProgram1


testInput =
    """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""


testInput2 =
    """snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""


input =
    """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 952
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""
