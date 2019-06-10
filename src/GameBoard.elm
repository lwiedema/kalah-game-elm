module GameBoard exposing (BoardPosition(..), GameBoard, SowingState(..), addAllRemainingSeedsToStore, getRowForPlayer, getStoreForPlayer, handleSeedInEmptyHouse, initalBoard, isRowEmpty, nextPosition, numberOfSeedsInHouse, pickSeeds, sowAtPosition)

import Html.Attributes exposing (placeholder, rows)
import List exposing (take)
import Lists exposing (setElementAt)
import Player exposing (Player(..), togglePlayer)
import Settings exposing (Settings)



-- BEGIN datatypes


type alias GameBoard =
    { rows : ( Row, Row )
    , stores : ( Store, Store )
    , sowingState : SowingState
    }


type alias Store =
    -- big pit, end zone
    Int


type alias Row =
    List House


type alias House =
    -- small pit
    Int


type SowingState
    = NotSowing
    | Sowing SowingInfo
    | SowingFinished Player
    | HandleLastSeedInEmptyHouse Player Int


type alias SowingInfo =
    { playerSowing : Player
    , seedsToSow : Int
    , position : BoardPosition
    }


type BoardPosition
    = RowPos Player Int
    | StorePos Player



-- END datatypes
-- BEGIN create inital game-board


buildBoard : Settings -> GameBoard
buildBoard settings =
    { rows =
        ( createRow settings.numberOfHouses settings.numberOfSeeds
        , createRow settings.numberOfHouses settings.numberOfSeeds
        )
    , stores = ( 0, 0 )
    , sowingState = NotSowing
    }


initalBoard : GameBoard
initalBoard =
    buildBoard Settings.defaultSettings


createRow : Int -> Int -> Row
createRow =
    Lists.repeat



-- END create inital game-board


addAllRemainingSeedsToStore : GameBoard -> Settings -> GameBoard
addAllRemainingSeedsToStore board settings =
    let
        seedsInRowOne =
            List.foldr (+) 0 (getRowForPlayer board One)

        seedsInRowTwo =
            List.foldr (+) 0 (getRowForPlayer board Two)

        playerGettingSeeds =
            if
                (seedsInRowOne == 0 && settings.lastSeedsForFinishingPlayer)
                    || (seedsInRowTwo == 0 && not settings.lastSeedsForFinishingPlayer)
            then
                One

            else
                Two
    in
    board
        |> setStoreForPlayer playerGettingSeeds (getStoreForPlayer board playerGettingSeeds + seedsInRowOne + seedsInRowTwo)
        |> removeAllSeedsFromHouses


removeAllSeedsFromHouses : GameBoard -> GameBoard
removeAllSeedsFromHouses gameBoard =
    { gameBoard
        | rows = ( List.map (\_ -> 0) (getRowForPlayer gameBoard One), List.map (\_ -> 0) (getRowForPlayer gameBoard Two) )
    }


nextPosition : Settings -> Player -> BoardPosition -> BoardPosition
nextPosition settings playerOnTurn position =
    -- find next seeding position
    case position of
        RowPos player posInRow ->
            if posInRow + 1 >= settings.numberOfHouses then
                if playerOnTurn == player then
                    StorePos playerOnTurn

                else
                    RowPos playerOnTurn 0

            else
                RowPos player (posInRow + 1)

        StorePos player ->
            RowPos (togglePlayer player) 0


addSeedsToStore : Player -> Int -> GameBoard -> GameBoard
addSeedsToStore player numOfSeeds board =
    setStoreForPlayer player (getStoreForPlayer board player + numOfSeeds) board


sowAtPosition : GameBoard -> BoardPosition -> GameBoard
sowAtPosition board position =
    -- add one seed to position
    case position of
        RowPos player int ->
            setRowForPlayer player (sowSeedToHouse int (getRowForPlayer board player)) board

        StorePos player ->
            setStoreForPlayer player (getStoreForPlayer board player + 1) board


sowSeedToHouse : Int -> Row -> Row
sowSeedToHouse pos row =
    case Lists.elementAt row pos of
        Just x ->
            Lists.setElementAt row pos (x + 1)

        Nothing ->
            row


handleSeedInEmptyHouse : GameBoard -> Settings -> Player -> Int -> GameBoard
handleSeedInEmptyHouse board settings playerOnTurn seedingPos =
    let
        otherPlayersRow =
            getRowForPlayer board (togglePlayer playerOnTurn)
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
            (togglePlayer playerOnTurn)
            (settings.numberOfHouses - 1 - seedingPos)


numberOfSeedsInHouse : Row -> Int -> Int
numberOfSeedsInHouse row pos =
    case Lists.elementAt row pos of
        Just x ->
            x

        Nothing ->
            0


pickSeeds : Player -> Int -> GameBoard -> GameBoard
pickSeeds player pos board =
    -- removes all seeds from house
    setRowForPlayer player (Lists.setElementAt (getRowForPlayer board player) pos 0) board


isRowEmpty : Row -> Bool
isRowEmpty row =
    not (Lists.any (\x -> not (x == 0)) row)



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
