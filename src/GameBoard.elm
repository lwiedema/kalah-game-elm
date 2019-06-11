module GameBoard exposing (BoardPosition(..), GameBoard, SowingState(..), addAllSeedsInRow, getFinalScore, getRowForPlayer, getStoreForPlayer, handleSeedInEmptyHouse, initalBoard, isRowEmpty, nextPosition, numberOfSeedsInHouse, pickSeeds, resetAllJustSown, sowAtPosition)

import Html.Attributes exposing (placeholder, rows)
import List exposing (take)
import Lists exposing (setElementAt)
import Player exposing (Player(..), togglePlayer)
import Settings exposing (Settings)
import String exposing (toInt)



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
    List House


type alias House =
    -- small pit
    { seeds : Int
    , justSownTo : Bool
    }


type SowingState
    = NotSowing
    | Sowing SowingInfo
    | SowingFinished Player Bool -- Bool-Flag whether player has one more turn
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


addAllSeedsInRow : Row -> Int -> Int
addAllSeedsInRow row seeds =
    case row of
        [] ->
            seeds

        h :: hs ->
            h.seeds + addAllSeedsInRow hs seeds


getFinalScore : GameBoard -> Settings -> ( Int, Int )
getFinalScore board settings =
    let
        seedsInRowOne =
            addAllSeedsInRow (getRowForPlayer board One) 0

        seedsInRowTwo =
            addAllSeedsInRow (getRowForPlayer board Two) 0

        playerGettingSeeds =
            if
                (seedsInRowOne == 0 && settings.lastSeedsForFinishingPlayer)
                    || (seedsInRowTwo == 0 && not settings.lastSeedsForFinishingPlayer)
            then
                One

            else
                Two

        seedsInStoreOne =
            (getStoreForPlayer board One).seeds

        seedsInStoreTwo =
            (getStoreForPlayer board Two).seeds
    in
    case playerGettingSeeds of
        One ->
            ( seedsInStoreOne + seedsInRowOne + seedsInRowTwo, seedsInStoreTwo )

        Two ->
            ( seedsInStoreOne, seedsInStoreTwo + seedsInRowOne + seedsInRowTwo )


removeAllSeedsFromHouses : GameBoard -> GameBoard
removeAllSeedsFromHouses gameBoard =
    { gameBoard
        | rows =
            ( List.map
                (\house -> { house | seeds = 0 })
                (getRowForPlayer gameBoard One)
            , List.map
                (\house -> { house | seeds = 0 })
                (getRowForPlayer gameBoard Two)
            )
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
    let
        store =
            getStoreForPlayer board player
    in
    setStoreForPlayer player { seeds = store.seeds + numOfSeeds, justSownTo = True } board


sowAtPosition : GameBoard -> BoardPosition -> GameBoard
sowAtPosition board position =
    -- add one seed to position
    case position of
        RowPos player int ->
            setRowForPlayer player (sowSeedToHouse int (getRowForPlayer board player)) board

        StorePos player ->
            addSeedsToStore player 1 board


sowSeedToHouse : Int -> Row -> Row
sowSeedToHouse pos row =
    case Lists.elementAt row pos of
        Just house ->
            Lists.setElementAt row pos { justSownTo = True, seeds = house.seeds + 1 }

        Nothing ->
            row


resetAllJustSown : GameBoard -> GameBoard
resetAllJustSown board =
    { board
        | rows =
            ( resetSownStateInRow (getRowForPlayer board One)
            , resetSownStateInRow (getRowForPlayer board Two)
            )
        , stores =
            ( resetSownStateInStore (getStoreForPlayer board One)
            , resetSownStateInStore (getStoreForPlayer board Two)
            )
    }


resetSownStateInStore : Store -> Store
resetSownStateInStore store =
    { store | justSownTo = False }


resetSownStateInRow : Row -> Row
resetSownStateInRow row =
    List.map (\house -> { house | justSownTo = False }) row


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
        Just house ->
            house.seeds

        Nothing ->
            0


pickSeeds : Player -> Int -> GameBoard -> GameBoard
pickSeeds player pos board =
    -- removes all seeds from house
    case Lists.elementAt (getRowForPlayer board player) pos of
        Just house ->
            setRowForPlayer player (Lists.setElementAt (getRowForPlayer board player) pos { house | seeds = 0 }) board

        Nothing ->
            board


isRowEmpty : Row -> Bool
isRowEmpty row =
    not (Lists.any (\house -> not (house.seeds == 0)) row)



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
