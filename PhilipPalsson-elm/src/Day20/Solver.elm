module Day20.Solver exposing (solve1, solve2)

import Array exposing (Array)
import Day20.Input
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra
import Maybe.Extra


type alias Tile =
    { cells : List (List Bool)
    , id : Int
    }


parseTile : String -> Maybe Tile
parseTile string =
    case String.lines string of
        head :: rest ->
            Maybe.map
                (Tile (rest |> List.map (String.split "" >> List.map ((==) "#"))))
                (String.replace "Tile " "" head
                    |> String.replace ":" ""
                    |> String.toInt
                )

        _ ->
            Nothing


type Side
    = Top
    | Right
    | Bottom
    | Left


type alias Spot =
    { matchSide : Side, toMatch : List Bool, pos : ( Int, Int ) }


type alias State =
    { availableSpots : List Spot, placed : Dict ( Int, Int ) Tile }


topRow : List (List Bool) -> List Bool
topRow =
    List.head >> Maybe.withDefault []


bottomRow : List (List Bool) -> List Bool
bottomRow =
    List.Extra.last >> Maybe.withDefault []


leftRow : List (List Bool) -> List Bool
leftRow =
    List.filterMap List.head


rightRow : List (List Bool) -> List Bool
rightRow =
    List.filterMap List.Extra.last


allMutations : Tile -> List Tile
allMutations tile =
    [ tile
    , tile |> rotateTile
    , tile |> rotateTile |> rotateTile
    , tile |> rotateTile |> rotateTile |> rotateTile
    , tile |> flipX
    , tile |> rotateTile |> flipX
    , tile |> rotateTile |> rotateTile |> flipX
    , tile |> rotateTile |> rotateTile |> rotateTile |> flipX
    ]


rotateTile : Tile -> Tile
rotateTile tile =
    { tile | cells = List.Extra.transpose tile.cells |> List.map List.reverse }


flipX : Tile -> Tile
flipX tile =
    { tile | cells = List.map List.reverse tile.cells }


findSpot : Tile -> List Spot -> Maybe ( Tile, Spot )
findSpot tile spots =
    allMutations tile
        |> List.foldl
            (\t acc ->
                if acc == Nothing then
                    List.foldl
                        (\spot innerAcc ->
                            if innerAcc == Nothing then
                                let
                                    match =
                                        case spot.matchSide of
                                            Top ->
                                                spot.toMatch == topRow t.cells

                                            Right ->
                                                spot.toMatch == rightRow t.cells

                                            Bottom ->
                                                spot.toMatch == bottomRow t.cells

                                            Left ->
                                                spot.toMatch == leftRow t.cells
                                in
                                if match then
                                    Just ( t, spot )

                                else
                                    Nothing

                            else
                                innerAcc
                        )
                        Nothing
                        spots

                else
                    acc
            )
            Nothing


placeTiles : List Tile -> State -> State
placeTiles tiles state =
    let
        ( newState, tilesLeft ) =
            List.foldl
                (\tile ( currentState, currentTilesToDo ) ->
                    case findSpot tile currentState.availableSpots of
                        Just ( tileVersionThatFit, spot ) ->
                            ( placeTile currentState tileVersionThatFit spot.pos, currentTilesToDo )

                        Nothing ->
                            ( currentState, tile :: currentTilesToDo )
                )
                ( state, [] )
                tiles
    in
    if List.isEmpty tilesLeft then
        newState

    else
        placeTiles tilesLeft newState


placeTile : State -> Tile -> ( Int, Int ) -> State
placeTile state tile (( x, y ) as tilePos) =
    let
        newPlaced =
            Dict.insert tilePos tile state.placed
    in
    { state
        | placed = newPlaced
        , availableSpots =
            ([ { pos = ( x, y + 1 ), matchSide = Bottom, toMatch = topRow tile.cells }
             , { pos = ( x + 1, y ), matchSide = Left, toMatch = rightRow tile.cells }
             , { pos = ( x, y - 1 ), matchSide = Top, toMatch = bottomRow tile.cells }
             , { pos = ( x - 1, y ), matchSide = Right, toMatch = leftRow tile.cells }
             ]
                ++ state.availableSpots
            )
                |> List.Extra.filterNot (\spot -> Dict.member spot.pos newPlaced)
    }


setCellValue : Int -> Int -> Bool -> Array (Array Bool) -> Array (Array Bool)
setCellValue x y value array =
    Array.set y
        (Array.get y array
            |> Maybe.map (Array.set x value)
            |> Maybe.withDefault Array.empty
        )
        array


insertTileCells : Int -> Int -> Array (Array Bool) -> Tile -> Array (Array Bool)
insertTileCells xOffset yOffset array tile =
    List.indexedMap
        (\rowIndex row ->
            List.indexedMap
                (\cellIndex cell ->
                    ( cellIndex, rowIndex, cell )
                )
                row
        )
        tile.cells
        |> List.concat
        |> List.foldl
            (\( x, y, value ) res ->
                setCellValue (xOffset + x) (yOffset + y) value res
            )
            array


dropEdges : Tile -> Tile
dropEdges tile =
    { tile
        | cells =
            tile.cells
                |> List.tail
                |> Maybe.withDefault []
                |> List.reverse
                |> List.tail
                |> Maybe.withDefault []
                |> List.reverse
                |> List.map
                    (List.tail
                        >> Maybe.withDefault []
                        >> List.reverse
                        >> List.tail
                        >> Maybe.withDefault []
                        >> List.reverse
                    )
    }


findTileOrdering : Tile -> List Tile -> List Tile
findTileOrdering firstTile restOfTiles =
    placeTile { availableSpots = [], placed = Dict.empty } firstTile ( 0, 0 )
        |> placeTiles restOfTiles
        |> .placed
        |> Dict.toList
        |> List.sortBy (\( ( x, y ), _ ) -> y * 1000 + x)
        |> List.map Tuple.second
        |> List.map dropEdges
        |> List.map (\tile -> { tile | cells = List.reverse tile.cells })


solve2 =
    let
        tiles =
            Day20.Input.input
                |> String.split "\n\n"
                |> List.filterMap parseTile
    in
    case tiles of
        head :: tail ->
            let
                cellArray =
                    findTileOrdering head tail
                        |> List.Extra.greedyGroupsOf 12
                        |> List.Extra.indexedFoldl
                            (\groupIndex group res ->
                                List.Extra.indexedFoldl
                                    (\cellIndex tile innerRes ->
                                        insertTileCells
                                            (cellIndex * 8)
                                            (groupIndex * 8)
                                            innerRes
                                            tile
                                    )
                                    res
                                    group
                            )
                            (Array.initialize (12 * 8) (always (Array.initialize (12 * 8) (always False))))

                allHashSigns : List ( Int, Int )
                allHashSigns =
                    Array.indexedMap
                        (\x row ->
                            Array.indexedMap
                                (\y c ->
                                    if c then
                                        Just ( x, y )

                                    else
                                        Nothing
                                )
                                row
                                |> Array.toList
                                |> List.filterMap identity
                        )
                        cellArray
                        |> Array.toList
                        |> List.concat

                hashSignsPartOfMonster =
                    List.concatMap
                        (\monsterVariant ->
                            hashSignsPartOfMonsterVariant monsterVariant cellArray
                        )
                        monsterVariants
            in
            List.length allHashSigns - List.length hashSignsPartOfMonster

        _ ->
            0


solve1 =
    let
        tiles =
            Day20.Input.input
                |> String.split "\n\n"
                |> List.filterMap parseTile
    in
    case tiles of
        head :: tail ->
            let
                tileOrdering =
                    findTileOrdering head tail
            in
            [ List.Extra.getAt 0 tileOrdering
            , List.Extra.getAt 11 tileOrdering
            , List.Extra.getAt 132 tileOrdering
            , List.Extra.getAt 143 tileOrdering
            ]
                |> List.filterMap identity
                |> List.map .id
                |> Debug.log "dfsd"
                |> List.product

        _ ->
            0


getCell : Int -> Int -> Array (Array Bool) -> Maybe Bool
getCell x y array =
    Array.get y array |> Maybe.map (Array.get x) |> Maybe.Extra.join


hashSignsPartOfMonsterVariant : List (List Bool) -> Array (Array Bool) -> List ( Int, Int )
hashSignsPartOfMonsterVariant monsterVariant cellArray =
    let
        monsterVariantArray =
            List.map Array.fromList monsterVariant |> Array.fromList

        monsterHeight =
            List.length monsterVariant - 1

        monsterWidth =
            (List.head monsterVariant |> Maybe.map List.length |> Maybe.withDefault 0) - 1

        monsterOffsets : List ( Int, Int )
        monsterOffsets =
            List.concatMap
                (\x ->
                    List.map (\y -> ( x, y ))
                        (List.range 0 monsterHeight)
                )
                (List.range 0 monsterWidth)
    in
    Array.indexedMap
        (\x row ->
            Array.indexedMap
                (\y _ ->
                    let
                        foundMonster =
                            List.all
                                (\( innerX, innerY ) ->
                                    case getCell innerX innerY monsterVariantArray of
                                        Just True ->
                                            getCell (innerX + x) (innerY + y) cellArray == Just True

                                        Just False ->
                                            True

                                        Nothing ->
                                            False
                                )
                                monsterOffsets
                    in
                    if foundMonster then
                        Just
                            (List.filter
                                (\( innerX, innerY ) ->
                                    getCell innerX innerY monsterVariantArray == Just True
                                )
                                monsterOffsets
                            )

                    else
                        Nothing
                )
                row
        )
        cellArray
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.filterMap identity
        |> List.concat


monsterString =
    """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """


monsterVariants : List (List (List Bool))
monsterVariants =
    let
        monster =
            String.lines monsterString |> List.map (String.split "" >> List.map ((==) "#"))

        rotate l =
            List.Extra.transpose l |> List.map List.reverse

        flip l =
            List.map List.reverse l
    in
    [ monster
    , monster |> rotate
    , monster |> rotate |> rotate
    , monster |> rotate |> rotate |> rotate
    , monster |> flip
    , monster |> rotate |> flip
    , monster |> rotate |> rotate |> flip
    , monster |> rotate |> rotate |> rotate |> flip
    ]


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
