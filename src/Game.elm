module Game exposing (Game, State(..), findWinner, nextSowingStep, startSowingSeeds)

import GameBoard exposing (..)
import Player exposing (..)
import Settings exposing (Settings)


type alias Game =
    { state : State
    , board : GameBoard
    , settings : Settings
    }


type State
    = Turn Player
    | End Winner


findWinner : Game -> Winner
findWinner game =
    if getStoreForPlayer game.board One == getStoreForPlayer game.board Two then
        Drawn

    else if getStoreForPlayer game.board One > getStoreForPlayer game.board Two then
        Winner One

    else
        Winner Two


startSowingSeeds : Game -> Player -> Int -> Game
startSowingSeeds game player position =
    case game.board.sowingState of
        NotSowing ->
            -- Starting-point for distributing seeds from one house
            let
                -- setting inital state
                tmpSowingState =
                    Sowing
                        { playerSowing = player
                        , seedsToSow = numberOfSeedsInHouse (getRowForPlayer game.board player) position
                        , position = nextPosition game.settings player (RowPos player position)
                        }

                -- picking up seeds from house
                newBoard =
                    pickSeeds player position game.board
            in
            -- return changed game
            { game | board = { newBoard | sowingState = tmpSowingState } }

        _ ->
            -- should never get to this point
            game


nextSowingStep : Game -> Game
nextSowingStep game =
    case game.board.sowingState of
        SowingFinished player ->
            let
                lastSowingPlayersRow =
                    getRowForPlayer game.board player

                otherPlayersRow =
                    getRowForPlayer game.board (togglePlayer player)
            in
            if isRowEmpty lastSowingPlayersRow || isRowEmpty otherPlayersRow then
                { game | board = addAllRemainingSeedsToStore game.board game.settings, state = End (findWinner game) }

            else
                let
                    b =
                        game.board
                in
                { game | board = { b | sowingState = NotSowing } }

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
                                        , position = nextPosition game.settings sowingInfo.playerSowing sowingInfo.position
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
                                    handleSeedInEmptyHouse boardAfterSowing game.settings player posInRow
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

        _ ->
            -- should never get to this point
            game
