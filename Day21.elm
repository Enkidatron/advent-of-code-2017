module Day21 exposing (..)

import Matrix exposing (..)
import Stream
import Dict exposing (..)
import Parser exposing (..)
import Dict.Extra


type alias Painting =
    Matrix Pixel


type Pixel
    = On
    | Off
    | Nonsense


type alias Rule =
    ( Painting, Painting )


initialPainting =
    Matrix.fromList [ [ Off, On, Off ], [ Off, Off, On ], [ On, On, On ] ]


part1 input =
    let
        rulesByLiveCount =
            parseRulesForPart1 input
    in
        Stream.iterate initialPainting (expandPainting rulesByLiveCount)
            |> streamDrop 5
            |> Stream.next
            |> Tuple.second
            |> Maybe.map getLiveCount


part2recursive input =
    let
        rules =
            parseRulesForPart2 input
    in
        part2Helper rules 18 initialPainting
            |> getLiveCount


part2Helper : Dict ( Int, Int ) (List Rule) -> Int -> Painting -> Painting
part2Helper rules stepsLeft painting =
    if stepsLeft == 0 then
        painting
    else
        part2Helper rules (stepsLeft - 1) (expandPainting2 rules painting)


part2stream input =
    let
        rules =
            parseRulesForPart2 input
    in
        Stream.iterate initialPainting (expandPainting2 rules)
            |> streamDrop 18
            |> Stream.next
            |> Tuple.second
            |> Maybe.map getLiveCount


streamDrop toDrop stream =
    if toDrop <= 0 then
        stream
    else
        streamDrop (toDrop - 1) <| Tuple.first <| Stream.next stream


parseRules input =
    run rulesParser input
        |> Result.mapError (Debug.log "Parse Error")
        |> Result.withDefault []


parseRulesForPart1 =
    parseRules
        >> Dict.Extra.groupBy (Tuple.first >> getLiveCount)


parseRulesForPart2 =
    parseRules
        >> Dict.Extra.groupBy ((\( input, output ) -> ( getLiveCount input, getCornerLiveCount input )))


parsePainting =
    run paintingParser >> Result.withDefault (Matrix.fromList [])


rulesParser : Parser (List Rule)
rulesParser =
    Parser.repeat zeroOrMore ruleParser


ruleParser : Parser Rule
ruleParser =
    succeed (,)
        |= paintingParser
        |. symbol " => "
        |= paintingParser
        |. keep zeroOrMore ((==) '\n')


paintingParser : Parser Painting
paintingParser =
    keep oneOrMore (\char -> char == '.' || char == '#' || char == '/')
        |> Parser.map (String.split "/" >> List.map String.toList >> Matrix.fromList >> Matrix.map toPixel)


toPixel : Char -> Pixel
toPixel char =
    case char of
        '.' ->
            Off

        '#' ->
            On

        _ ->
            Nonsense


getLiveCount =
    Matrix.flatten >> List.filter ((==) On) >> List.length


getCornerLiveCount painting =
    [ ( 0, 0 )
    , ( 0, Matrix.colCount painting - 1 )
    , ( Matrix.rowCount painting - 1, 0 )
    , ( Matrix.rowCount painting - 1, Matrix.colCount painting - 1 )
    ]
        |> List.filterMap (\pos -> Matrix.get pos painting)
        |> List.filter ((==) On)
        |> List.length


getNonsenseCount =
    Matrix.flatten >> List.filter ((==) Nonsense) >> List.length


printAsRule =
    Matrix.toList >> List.map ((List.map pixelToChar) >> String.fromList) >> String.join "/"


pixelToChar char =
    case char of
        On ->
            '#'

        Off ->
            '.'

        Nonsense ->
            '?'


expandPainting : Dict Int (List Rule) -> Painting -> Painting
expandPainting rulesByLiveCount painting =
    let
        pieces =
            splitPainting painting

        newPieces =
            Matrix.map (applyRules rulesByLiveCount) pieces
    in
        assemblePieces newPieces


expandPainting2 : Dict ( Int, Int ) (List Rule) -> Painting -> Painting
expandPainting2 rules painting =
    splitPainting painting
        |> Matrix.map (applyRules2 rules)
        |> assemblePieces


splitPainting : Painting -> Matrix Painting
splitPainting painting =
    let
        splitSize =
            if Matrix.colCount painting % 2 == 0 then
                2
            else
                3

        metaMatrixSize =
            (Matrix.colCount painting) // splitSize
    in
        Matrix.matrix metaMatrixSize metaMatrixSize (getPiece painting splitSize)


getPiece : Painting -> Int -> Location -> Painting
getPiece painting splitSize ( row, col ) =
    let
        rowOffset =
            row * splitSize

        colOffset =
            col * splitSize
    in
        Matrix.matrix splitSize splitSize (getPixelWithOffset painting ( rowOffset, colOffset ))


getPixelWithOffset : Painting -> Location -> Location -> Pixel
getPixelWithOffset painting ( offsetRow, offsetCol ) ( row, col ) =
    Matrix.get ( offsetRow + row, offsetCol + col ) painting
        |> Maybe.withDefault Nonsense


assemblePieces : Matrix Painting -> Painting
assemblePieces metaPainting =
    let
        pieceSize =
            Matrix.get ( 0, 0 ) metaPainting
                |> Maybe.map Matrix.colCount
                |> Maybe.withDefault 1

        newTotalSize =
            pieceSize * (Matrix.colCount metaPainting)

        findPixelInPiece : Location -> Pixel
        findPixelInPiece ( row, col ) =
            let
                outerRow =
                    row
                        // pieceSize

                innerRow =
                    row
                        % pieceSize

                outerCol =
                    col
                        // pieceSize

                innerCol =
                    col
                        % pieceSize
            in
                Matrix.get ( outerRow, outerCol ) metaPainting
                    |> Maybe.andThen (\piece -> Matrix.get ( innerRow, innerCol ) piece)
                    |> Maybe.withDefault Nonsense
    in
        Matrix.matrix newTotalSize newTotalSize findPixelInPiece


applyRules : Dict Int (List Rule) -> Painting -> Painting
applyRules rulesByLiveCount painting =
    let
        liveCount =
            getLiveCount painting

        ruleList =
            Dict.get liveCount rulesByLiveCount
                |> Maybe.withDefault []

        maybeRule =
            ruleList |> List.filter (ruleMatches painting) |> complainIfTooManyMatches |> List.head
    in
        maybeRule |> Maybe.map Tuple.second |> Maybe.withDefault painting


ruleMatches : Painting -> Rule -> Bool
ruleMatches painting ( input, _ ) =
    input
        |> makeVariations
        |> List.filter ((==) painting)
        |> List.length
        |> ((/=) 0)


applyRules2 : Dict ( Int, Int ) (List Rule) -> Painting -> Painting
applyRules2 rules painting =
    Dict.get ( getLiveCount painting, getCornerLiveCount painting ) rules
        |> Maybe.withDefault []
        |> List.filter (ruleMatches painting)
        |> complainIfTooManyMatches
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault painting


complainIfTooManyMatches list =
    if List.length list /= 1 then
        Debug.log "wrong number of matches" list
    else
        list


makeVariations : Painting -> List Painting
makeVariations painting =
    if Matrix.colCount painting == 2 then
        makeVariations2 painting
    else
        makeVariations3 painting


makeVariations2 : Painting -> List Painting
makeVariations2 painting =
    [ painting
    , flipRows painting
    , flipCols painting
    , flipRows <| flipCols painting
    , spinRight2 painting
    , spinRight2 <| spinRight2 <| spinRight2 painting
    , spinRight2 <| flipRows painting
    , spinRight2 <| flipCols painting
    ]


flipRows =
    Matrix.toList >> List.reverse >> Matrix.fromList


flipCols =
    Matrix.toList >> List.map List.reverse >> Matrix.fromList


spinRight2 painting =
    Matrix.fromList
        [ [ ( 1, 0 ), ( 0, 0 ) ]
        , [ ( 1, 1 ), ( 0, 1 ) ]
        ]
        |> Matrix.map (\pos -> Matrix.get pos painting |> Maybe.withDefault Nonsense)


makeVariations3 painting =
    [ painting
    , flipRows painting
    , flipCols painting
    , flipRows <| flipCols painting
    , spinRight3 painting
    , spinRight3 <| spinRight3 <| spinRight3 painting
    , spinRight3 <| flipRows painting
    , spinRight3 <| flipCols painting
    ]


spinRight3 painting =
    Matrix.fromList
        [ [ ( 2, 0 ), ( 1, 0 ), ( 0, 0 ) ]
        , [ ( 2, 1 ), ( 1, 1 ), ( 0, 1 ) ]
        , [ ( 2, 2 ), ( 1, 2 ), ( 0, 2 ) ]
        ]
        |> Matrix.map (\pos -> Matrix.get pos painting |> Maybe.withDefault Nonsense)
