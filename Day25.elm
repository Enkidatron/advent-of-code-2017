module Day25 exposing (..)


type alias Model =
    { tapeLeft : List Bool
    , currentVal : Bool
    , tapeRight : List Bool
    , state : State
    }


type State
    = A
    | B
    | C
    | D
    | E
    | F


type Dir
    = Left
    | Right


nextMove : State -> Bool -> ( Bool, Dir, State )
nextMove move currentVal =
    case ( move, currentVal ) of
        ( A, False ) ->
            ( True, Right, B )

        ( A, True ) ->
            ( False, Left, C )

        ( B, False ) ->
            ( True, Left, A )

        ( B, True ) ->
            ( True, Right, D )

        ( C, False ) ->
            ( True, Right, A )

        ( C, True ) ->
            ( False, Left, E )

        ( D, False ) ->
            ( True, Right, A )

        ( D, True ) ->
            ( False, Right, B )

        ( E, False ) ->
            ( True, Left, F )

        ( E, True ) ->
            ( True, Left, C )

        ( F, False ) ->
            ( True, Right, D )

        ( F, True ) ->
            ( True, Right, A )


steps =
    12173597


model =
    Model [] False [] A


step : Model -> Model
step model =
    let
        ( valToWrite, dir, newState ) =
            nextMove model.state model.currentVal

        ( nextLeft, nextVal, nextRight ) =
            case dir of
                Left ->
                    ( List.drop 1 model.tapeLeft, List.head model.tapeLeft |> Maybe.withDefault False, valToWrite :: model.tapeRight )

                Right ->
                    ( valToWrite :: model.tapeLeft, List.head model.tapeRight |> Maybe.withDefault False, List.drop 1 model.tapeRight )
    in
        { tapeLeft = nextLeft, currentVal = nextVal, tapeRight = nextRight, state = newState }


part1 : Int -> Model -> Int
part1 stepsLeft model =
    if stepsLeft <= 0 then
        (countTrue (model.currentVal :: model.tapeLeft) + countTrue model.tapeRight)
    else
        part1 (stepsLeft - 1) (step model)


countTrue : List Bool -> Int
countTrue =
    List.filter identity >> List.length
