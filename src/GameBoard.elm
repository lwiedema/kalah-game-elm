module GameBoard exposing (BoardPosition(..), GameBoard, SowingState(..), getFinalScore, getRowForPlayer, getStoreForPlayer, handleSeedInEmptyHouse, initalBoard, isRowEmpty, nextPosition, numberOfSeedsInHouse, pickSeeds, resetSowingStates, sowAtPosition)

import Lists exposing (setElementAt)
import Player exposing (Player(..))
import Settings exposing (Settings)



-- BEGIN datatypes


type alias GameBoard =
    { rows : ( Row, Row )
    , stores : ( Store, Store )
    , sowingState : SowingState
    }


type alias Store =
    -- big pit, end zone
    House


type alias Row =
    -- small pits
    List House


type alias House =
    { seeds : Int -- number of seeds in house
    , justSownTo : Bool -- flag if there was recently added a seed
    }


type SowingState
    = NotSowing
    | Sowing SowingInfo
    | SowingFinished Player Bool -- bool-flag if player has one more turn
    | HandleLastSeedInEmptyHouse Player Int


type alias SowingInfo =
    { playerSowing : Player
    , seedsToSow : Int -- number of seeds left to sow
    , position : BoardPosition -- position where to sow next
    }


type BoardPosition
    = RowPos Player Int
    | StorePos Player



-- END datatypes
-- BEGIN create inital game-board


emptyHouse : House
emptyHouse =
    { seeds = 0, justSownTo = False }


buildBoard : Settings -> GameBoard
buildBoard settings =
    { rows =
        ( createRow settings.numberOfHouses settings.numberOfSeeds
        , createRow settings.numberOfHouses settings.numberOfSeeds
        )
    , stores = ( emptyHouse, emptyHouse )
    , sowingState = NotSowing
    }


initalBoard : GameBoard
initalBoard =
    buildBoard Settings.defaultSettings


createRow : Int -> Int -> Row
createRow houses seeds =
    Lists.repeat houses { seeds = seeds, justSownTo = False }



-- END create inital game-board
-- BEGIN functions "modifying" board


sowAtPosition : GameBoard -> BoardPosition -> GameBoard
sowAtPosition board position =
    -- add one seed to position
    case position of
        RowPos player pos ->
            setRowForPlayer
                player
                (sowSeedToHouse pos (getRowForPlayer board player))
                board

        StorePos player ->
            addSeedsToStore player 1 board


resetSowingStates : GameBoard -> GameBoard
resetSowingStates board =
    -- set justSownTo-States of both rows and stores to False
    -- as well as board-sowingState to NotSowing
    { board
        | rows =
            ( resetSownStateInRow (getRowForPlayer board One)
            , resetSownStateInRow (getRowForPlayer board Two)
            )
        , stores =
            ( resetSownStateInStore (getStoreForPlayer board One)
            , resetSownStateInStore (getStoreForPlayer board Two)
            )
        , sowingState = NotSowing
    }


handleSeedInEmptyHouse : GameBoard -> Settings -> Player -> Int -> GameBoard
handleSeedInEmptyHouse board settings playerOnTurn seedingPos =
    -- when last seed gets sown in an empty house in own row,
    -- this seed and all seeds from opposite (other player's) house
    -- are added to store
    let
        otherPlayersRow =
            getRowForPlayer board (Player.togglePlayer playerOnTurn)
    in
    board
        -- remove single seed
        |> pickSeeds playerOnTurn seedingPos
        -- add single seed to store
        |> addSeedsToStore playerOnTurn 1
        -- add other player's seeds to store
        |> addSeedsToStore
            playerOnTurn
            (numberOfSeedsInHouse otherPlayersRow (settings.numberOfHouses - 1 - seedingPos))
        -- remove seeds from other player's house
        |> pickSeeds
            (Player.togglePlayer playerOnTurn)
            (settings.numberOfHouses - 1 - seedingPos)


pickSeeds : Player -> Int -> GameBoard -> GameBoard
pickSeeds player pos board =
    -- removes all seeds from house
    case Lists.elementAt (getRowForPlayer board player) pos of
        Just house ->
            setRowForPlayer
                player
                (Lists.setElementAt
                    (getRowForPlayer board player)
                    pos
                    { house | seeds = 0 }
                )
                board

        Nothing ->
            board



-- END functions "modifying" board
-- BEGIN functions for getting information about board/row/...


numberOfSeedsInHouse : Row -> Int -> Int
numberOfSeedsInHouse row pos =
    case Lists.elementAt row pos of
        Just house ->
            house.seeds

        Nothing ->
            0


isRowEmpty : Row -> Bool
isRowEmpty row =
    not (Lists.any (\house -> not (house.seeds == 0)) row)


numberOfSeedsInRow : Row -> Int
numberOfSeedsInRow row =
    List.foldr (\house -> (+) house.seeds) 0 row


getFinalScore : GameBoard -> Settings -> ( Int, Int )
getFinalScore board settings =
    -- finding out final score after game finished
    let
        -- getting number of seeds per row and store
        seedsInRowOne =
            numberOfSeedsInRow (getRowForPlayer board One)

        seedsInRowTwo =
            numberOfSeedsInRow (getRowForPlayer board Two)

        seedsInStoreOne =
            (getStoreForPlayer board One).seeds

        seedsInStoreTwo =
            (getStoreForPlayer board Two).seeds

        -- finding out who gets seeds which are still in row
        playerGettingSeeds =
            if
                -- depends on setting and who runs out of seeds first
                (seedsInRowOne == 0 && settings.lastSeedsForFinishingPlayer)
                    || (seedsInRowTwo == 0 && not settings.lastSeedsForFinishingPlayer)
            then
                One

            else
                Two
    in
    case playerGettingSeeds of
        One ->
            ( seedsInStoreOne + seedsInRowOne + seedsInRowTwo, seedsInStoreTwo )

        Two ->
            ( seedsInStoreOne, seedsInStoreTwo + seedsInRowOne + seedsInRowTwo )


nextPosition : Settings -> Player -> BoardPosition -> BoardPosition
nextPosition settings playerOnTurn position =
    -- find next sowing position
    case position of
        RowPos player posInRow ->
            if posInRow + 1 >= settings.numberOfHouses then
                -- last house in row
                if playerOnTurn == player then
                    -- own row: store comes next
                    StorePos playerOnTurn

                else
                    -- opponent's store not sown, continue in own row
                    RowPos playerOnTurn 0

            else
                RowPos player (posInRow + 1)

        StorePos player ->
            RowPos (Player.togglePlayer player) 0



-- END functions for getting information about board/row/...
-- BEGIN "private" helper functions


sowSeedToHouse : Int -> Row -> Row
sowSeedToHouse pos row =
    case Lists.elementAt row pos of
        Just house ->
            Lists.setElementAt row pos { justSownTo = True, seeds = house.seeds + 1 }

        Nothing ->
            row


addSeedsToStore : Player -> Int -> GameBoard -> GameBoard
addSeedsToStore player numOfSeeds board =
    let
        store =
            getStoreForPlayer board player
    in
    setStoreForPlayer
        player
        { seeds = store.seeds + numOfSeeds, justSownTo = True }
        board


resetSownStateInStore : Store -> Store
resetSownStateInStore store =
    { store | justSownTo = False }


resetSownStateInRow : Row -> Row
resetSownStateInRow row =
    List.map (\house -> { house | justSownTo = False }) row



-- END "private" helper functions
-- BEGIN "getter/setter" for row and store by player


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



-- END "getter/setter" for row and store by player
