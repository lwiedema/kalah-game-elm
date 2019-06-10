module Game exposing (Game, State(..), findWinner, nextSowingStep, startSowingSeeds)

import GameBoard exposing (BoardPosition(..), GameBoard, SowingState(..))
import Player exposing (Player(..), Winner(..))
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
    if GameBoard.getStoreForPlayer game.board One == GameBoard.getStoreForPlayer game.board Two then
        Drawn

    else if GameBoard.getStoreForPlayer game.board One > GameBoard.getStoreForPlayer game.board Two then
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
                        , seedsToSow = GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer game.board player) position
                        , position = GameBoard.nextPosition game.settings player (RowPos player position)
                        }

                -- picking up seeds from house
                newBoard =
                    GameBoard.pickSeeds player position game.board
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
                    GameBoard.getRowForPlayer game.board player

                otherPlayersRow =
                    GameBoard.getRowForPlayer game.board (Player.togglePlayer player)
            in
            if GameBoard.isRowEmpty lastSowingPlayersRow || GameBoard.isRowEmpty otherPlayersRow then
                { game | board = GameBoard.addAllRemainingSeedsToStore game.board game.settings, state = End (findWinner game) }

            else
                let
                    b =
                        game.board
                in
                { game | board = { b | sowingState = NotSowing } }

        HandleLastSeedInEmptyHouse player pos ->
            let
                boardAfter =
                    GameBoard.handleSeedInEmptyHouse game.board game.settings player pos
            in
            { game
                | state = Turn (Player.togglePlayer player)
                , board = { boardAfter | sowingState = SowingFinished player }
            }

        Sowing sowingInfo ->
            let
                -- sow seed at current house
                boardAfterSowing =
                    GameBoard.sowAtPosition game.board sowingInfo.position

                seedsToSowNext =
                    sowingInfo.seedsToSow - 1
            in
            if seedsToSowNext > 0 then
                --  sowNextSeed
                { game
                    | board =
                        { boardAfterSowing
                            | sowingState =
                                Sowing
                                    { sowingInfo
                                        | seedsToSow = seedsToSowNext
                                        , position = GameBoard.nextPosition game.settings sowingInfo.playerSowing sowingInfo.position
                                    }
                        }
                }

            else
                case sowingInfo.position of
                    RowPos player posInRow ->
                        if
                            player
                                == sowingInfo.playerSowing
                                && GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer boardAfterSowing player) posInRow
                                == 1
                            -- last seed sown to empty house
                        then
                            { game
                                | board = { boardAfterSowing | sowingState = HandleLastSeedInEmptyHouse player posInRow }
                            }

                        else
                            -- anderer Spieler ist dran
                            --  sowNextSeed
                            { game
                                | state = Turn (Player.togglePlayer sowingInfo.playerSowing)
                                , board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing }
                            }

                    StorePos player ->
                        -- player ist nochmal dran
                        --  sowNextSeed
                        { game | board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing } }

        _ ->
            -- should never get to this point
            game
