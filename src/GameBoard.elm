module GameBoard exposing (BoardPosition(..), GameBoard, House, Player(..), Row, SowingInfo, SowingState(..), Store, createRow, getRowForPlayer, getStoreForPlayer, initalBoard, isRowEmpty, nextPosition, numberOfHouses, numberOfSeeds, numberOfSeedsInHouse, pickSeeds, setRowForPlayer, setStoreForPlayer, sowAtPosition, sowNextSeed, sowSeedToHouse, startSowingSeeds, togglePlayer)

import Html.Attributes exposing (placeholder, rows)
import List exposing (take)
import Lists exposing (setElementAt)


numberOfHouses =
    6


numberOfSeeds =
    4


type alias GameBoard =
    { rows : ( Row, Row )
    , stores : ( Store, Store )
    , sowingState : SowingState
    }


type BoardPosition
    = RowPos Player Int
    | StorePos Player


type SowingState
    = NotSowing
    | Sowing SowingInfo


type alias SowingInfo =
    { playerSowing : Player
    , seedsToSow : Int
    , position : BoardPosition
    }


getRowForPlayer : GameBoard -> Player -> Row
getRowForPlayer board player =
    case player of
        One ->
            Tuple.first board.rows

        Two ->
            Tuple.second board.rows


setRowForPlayer : Player -> Row -> GameBoard -> GameBoard
setRowForPlayer player row board =
    case player of
        One ->
            { board | rows = ( row, Tuple.second board.rows ) }

        Two ->
            { board | rows = ( Tuple.first board.rows, row ) }


getStoreForPlayer : GameBoard -> Player -> Store
getStoreForPlayer board player =
    case player of
        One ->
            Tuple.first board.stores

        Two ->
            Tuple.second board.stores


setStoreForPlayer : Player -> Store -> GameBoard -> GameBoard
setStoreForPlayer player store board =
    case player of
        One ->
            { board | stores = ( store, Tuple.second board.stores ) }

        Two ->
            { board | stores = ( Tuple.first board.stores, store ) }


initalBoard : GameBoard
initalBoard =
    { rows = ( createRow numberOfHouses numberOfSeeds, createRow numberOfHouses numberOfSeeds )
    , stores = ( 0, 0 )
    , sowingState = NotSowing
    }


startSowingSeeds : GameBoard -> Player -> Int -> GameBoard
startSowingSeeds board player position =
    -- Starting point for distributing seeds from one house
    let
        -- setting inital state
        tmpSowingState =
            Sowing
                { playerSowing = player
                , seedsToSow = numberOfSeedsInHouse (getRowForPlayer board player) position
                , position = nextPosition player (RowPos player position)
                }

        -- picking up seeds from house
        newBoard =
            pickSeeds board player position
    in
    -- starting recursion
    sowNextSeed { newBoard | sowingState = tmpSowingState }


sowNextSeed : GameBoard -> GameBoard
sowNextSeed board =
    case board.sowingState of
        NotSowing ->
            board

        Sowing sowingInfo ->
            -- when all seeds are distributed, reset sowingState
            if sowingInfo.seedsToSow == 0 then
                { board | sowingState = NotSowing }

            else
                let
                    -- sow seed at current house
                    tmpBoard =
                        sowAtPosition board sowingInfo.position

                    seedsToSowNext =
                        sowingInfo.seedsToSow - 1
                in
                -- call function recursively with one seed less and next position
                if seedsToSowNext == 0 then
                    case sowingInfo.position of
                        RowPos player posInRow ->
                            if
                                player
                                    == sowingInfo.playerSowing
                                    && numberOfSeedsInHouse (getRowForPlayer board player) posInRow
                                    == 1
                            then
                                --TODO übertrage seeds in store
                                { tmpBoard | sowingState = NotSowing }

                            else
                                --TODO nichts
                                { tmpBoard | sowingState = NotSowing }

                        StorePos player ->
                            --TODO player ist nochmal dran
                            { tmpBoard | sowingState = NotSowing }

                else
                    sowNextSeed
                        { tmpBoard
                            | sowingState =
                                Sowing
                                    { sowingInfo
                                        | seedsToSow = seedsToSowNext
                                        , position = nextPosition sowingInfo.playerSowing sowingInfo.position
                                    }
                        }


sowAtPosition : GameBoard -> BoardPosition -> GameBoard
sowAtPosition board position =
    -- add one seed to position
    case position of
        RowPos player int ->
            setRowForPlayer player (sowSeedToHouse int (getRowForPlayer board player)) board

        StorePos player ->
            setStoreForPlayer player (getStoreForPlayer board player + 1) board


nextPosition : Player -> BoardPosition -> BoardPosition
nextPosition playerOnTurn position =
    -- find next seeding position
    case position of
        RowPos player posInRow ->
            if posInRow + 1 >= numberOfHouses then
                if playerOnTurn == player then
                    StorePos playerOnTurn

                else
                    RowPos playerOnTurn 0

            else
                RowPos player (posInRow + 1)

        StorePos player ->
            RowPos (togglePlayer player) 0


sowSeedToHouse : Int -> Row -> Row
sowSeedToHouse pos row =
    case Lists.elementAt row pos of
        Just x ->
            Lists.setElementAt row pos (x + 1)

        Nothing ->
            row


type alias Store =
    -- big pit, end zone
    Int


type alias House =
    -- small pit
    Int


type alias Row =
    List House


createRow : Int -> Int -> Row
createRow length initalSeedNumber =
    Lists.repeat length initalSeedNumber


numberOfSeedsInHouse : Row -> Int -> Int
numberOfSeedsInHouse row pos =
    case Lists.elementAt row pos of
        Just x ->
            x

        Nothing ->
            0


pickSeeds : GameBoard -> Player -> Int -> GameBoard
pickSeeds board player pos =
    setRowForPlayer player (Lists.setElementAt (getRowForPlayer board player) pos 0) board


isRowEmpty : Row -> Bool
isRowEmpty row =
    not (Lists.any (\x -> not (x == 0)) row)


type Player
    = One
    | Two


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        One ->
            Two

        Two ->
            One
