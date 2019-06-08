module GameBoard exposing (BoardPosition(..), Game, GameBoard, House, Player(..), Row, SowingInfo, SowingState(..), State(..), Store, Winner(..), addAllRemainingSeedsToStore, addSeedsToStore, createRow, findWinner, getRowForPlayer, getStoreForPlayer, handleSeedInEmptyHouse, initalBoard, isRowEmpty, nextPosition, numberOfHouses, numberOfSeeds, numberOfSeedsInHouse, pickSeeds, playerWhoRunsOutFirstGetsLastSeeds, removeAllSeedsFromHouses, setRowForPlayer, setStoreForPlayer, sowAtPosition, sowNextSeed, sowSeedToHouse, startSowingSeeds, togglePlayer)

import Html.Attributes exposing (placeholder, rows)
import List exposing (take)
import Lists exposing (setElementAt)


playerWhoRunsOutFirstGetsLastSeeds =
    False


numberOfHouses =
    6


numberOfSeeds =
    4


type alias Game =
    { state : State
    , board : GameBoard
    }


type Winner
    = Winner Player
    | Drawn


type State
    = Turn Player
    | End Winner


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
    | SowingFinished Player


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


startSowingSeeds : Game -> Player -> Int -> Game
startSowingSeeds game player position =
    -- Starting point for distributing seeds from one house
    let
        -- setting inital state
        tmpSowingState =
            Sowing
                { playerSowing = player
                , seedsToSow = numberOfSeedsInHouse (getRowForPlayer game.board player) position
                , position = nextPosition player (RowPos player position)
                }

        -- picking up seeds from house
        newBoard =
            pickSeeds player position game.board
    in
    -- starting recursion
    -- sowNextSeed { game | board = { newBoard | sowingState = tmpSowingState } }
    -- without recursion
    { game | board = { newBoard | sowingState = tmpSowingState } }


findWinner : Game -> Winner
findWinner game =
    if getStoreForPlayer game.board One == getStoreForPlayer game.board Two then
        Drawn

    else if getStoreForPlayer game.board One > getStoreForPlayer game.board Two then
        Winner One

    else
        Winner Two


addAllRemainingSeedsToStore : GameBoard -> GameBoard
addAllRemainingSeedsToStore board =
    let
        seedsInRowOne =
            List.foldr (+) 0 (getRowForPlayer board One)

        seedsInRowTwo =
            List.foldr (+) 0 (getRowForPlayer board Two)

        playerGettingSeeds =
            if
                (seedsInRowOne == 0 && playerWhoRunsOutFirstGetsLastSeeds)
                    || (seedsInRowTwo == 0 && not playerWhoRunsOutFirstGetsLastSeeds)
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


sowNextSeed : Game -> Game
sowNextSeed game =
    case game.board.sowingState of
        SowingFinished player ->
            let
                lastSowingPlayersRow =
                    getRowForPlayer game.board player

                otherPlayersRow =
                    getRowForPlayer game.board (togglePlayer player)
            in
            if isRowEmpty lastSowingPlayersRow || isRowEmpty otherPlayersRow then
                { game | board = addAllRemainingSeedsToStore game.board, state = End (findWinner game) }

            else
                let
                    b =
                        game.board
                in
                { game | board = { b | sowingState = NotSowing } }

        NotSowing ->
            game

        Sowing sowingInfo ->
            let
                -- sow seed at current house
                boardAfterSowing =
                    sowAtPosition game.board sowingInfo.position

                seedsToSowNext =
                    sowingInfo.seedsToSow - 1
            in
            -- call function recursively with one seed less and next position
            if seedsToSowNext > 0 then
                --  sowNextSeed
                { game
                    | board =
                        { boardAfterSowing
                            | sowingState =
                                Sowing
                                    { sowingInfo
                                        | seedsToSow = seedsToSowNext
                                        , position = nextPosition sowingInfo.playerSowing sowingInfo.position
                                    }
                        }
                }

            else
                case sowingInfo.position of
                    RowPos player posInRow ->
                        if
                            -- player
                            --     == sowingInfo.playerSowing
                            -- &&
                            numberOfSeedsInHouse (getRowForPlayer boardAfterSowing player) posInRow == 1
                        then
                            --TODO übertrage seeds in store
                            let
                                boardAfter =
                                    handleSeedInEmptyHouse boardAfterSowing player posInRow
                            in
                            --  sowNextSeed
                            { game
                                | board = { boardAfter | sowingState = SowingFinished sowingInfo.playerSowing }
                                , state = Turn (togglePlayer sowingInfo.playerSowing)
                            }

                        else
                            -- anderer Spieler ist dran
                            --  sowNextSeed
                            { game
                                | state = Turn (togglePlayer sowingInfo.playerSowing)
                                , board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing }
                            }

                    StorePos player ->
                        -- player ist nochmal dran
                        --  sowNextSeed
                        { game | board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing } }


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


addSeedsToStore : Player -> Int -> GameBoard -> GameBoard
addSeedsToStore player numOfSeeds board =
    setStoreForPlayer player (getStoreForPlayer board player + numOfSeeds) board


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


handleSeedInEmptyHouse : GameBoard -> Player -> Int -> GameBoard
handleSeedInEmptyHouse board playerOnTurn seedingPos =
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
            (numberOfSeedsInHouse otherPlayersRow (numberOfHouses - 1 - seedingPos))
        -- remove seeds from other player's house
        |> pickSeeds
            (togglePlayer playerOnTurn)
            (numberOfHouses - 1 - seedingPos)


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


pickSeeds : Player -> Int -> GameBoard -> GameBoard
pickSeeds player pos board =
    -- removes all seeds from house
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
