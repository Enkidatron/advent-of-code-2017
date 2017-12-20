module Day20 exposing (..)

import Parser exposing (..)
import Stream exposing (..)
import Set exposing (..)


type alias Vector3 =
    ( Int, Int, Int )


manhattanDistance : Vector3 -> Int
manhattanDistance ( x, y, z ) =
    abs x + abs y + abs z


vecSquared ( x, y, z ) =
    x * x + y * y + z * z


type alias Particle =
    { pos : Vector3
    , velocity : Vector3
    , accel : Vector3
    }


intParser =
    Parser.oneOf
        [ Parser.int
        , Parser.succeed ((*) -1)
            |. Parser.symbol "-"
            |= Parser.int
        ]


vec3Parser : Parser Vector3
vec3Parser =
    Parser.succeed (,,)
        |. symbol "<"
        |= intParser
        |. symbol ","
        |= intParser
        |. symbol ","
        |= intParser
        |. symbol ">"


particleParser : Parser Particle
particleParser =
    Parser.succeed Particle
        |. symbol "p="
        |= vec3Parser
        |. symbol ", v="
        |= vec3Parser
        |. symbol ", a="
        |= vec3Parser
        |. keep zeroOrMore ((==) '\n')


particlesParser =
    repeat zeroOrMore particleParser


parseInput : String -> List Particle
parseInput =
    Parser.run particlesParser
        >> Result.mapError (Debug.log "Parse Error")
        >> Result.withDefault []


tagWithIndex : List a -> List ( Int, a )
tagWithIndex =
    List.indexedMap (,)


smallestAcceleration =
    List.sortBy (Tuple.second >> .accel >> vecSquared)


part1 =
    parseInput >> tagWithIndex >> smallestAcceleration >> List.take 5



-- closest particle is the one with the smallest (absolute) acceleration.
-- if tied, the one with the lower starting velocity in the relevant direction stays slightly closer.


makePart2Stream particles =
    Stream.iterate particles stepSimulation


addVec : Vector3 -> Vector3 -> Vector3
addVec ( a, b, c ) ( x, y, z ) =
    ( a + x, b + y, c + z )


updateParticle : Particle -> Particle
updateParticle particle =
    let
        newVelocity =
            addVec particle.velocity particle.accel

        newPos =
            addVec particle.pos newVelocity
    in
        { particle | pos = newPos, velocity = newVelocity }


comparePositions : Particle -> ( Set Vector3, Set Vector3 ) -> ( Set Vector3, Set Vector3 )
comparePositions particle ( safe, collisions ) =
    if Set.member particle.pos collisions then
        ( safe, collisions )
    else if Set.member particle.pos safe then
        ( Set.remove particle.pos safe, Set.insert particle.pos safe )
    else
        ( Set.insert particle.pos safe, collisions )


stepSimulation : List Particle -> List Particle
stepSimulation particles =
    let
        updatedParticles =
            List.map updateParticle particles

        ( safePositions, collisionPositions ) =
            List.foldl comparePositions ( Set.empty, Set.empty ) updatedParticles
    in
        List.filter (.pos >> flip Set.member safePositions) updatedParticles


part2 =
    parseInput
        >> makePart2Stream
        -->> Stream.dropWhile stillSimulating
        -- I'm guessing that ten thousand cycles will resolve all collisions.
        >>
            streamDrop 10000
        >> Stream.next
        >> Tuple.second
        >> Maybe.map List.length


streamDrop toDrop =
    Stream.zip Stream.naturalNumbers
        >> Stream.dropWhile (Tuple.first >> (\index -> index <= toDrop))
        >> Stream.map Tuple.second
