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
    -- find out winner after game finished
    let
        finalScore =
            GameBoard.getFinalScore game.board game.settings
    in
    if Tuple.first finalScore == Tuple.second finalScore then
        Drawn

    else if Tuple.first finalScore > Tuple.second finalScore then
        Winner One finalScore

    else
        Winner Two finalScore


startSowingSeeds : Game -> Player -> Int -> Game
startSowingSeeds game player position =
    -- Starting-point for distributing seeds from one house
    case game.board.sowingState of
        NotSowing ->
            -- just double-checking if not sowing - should not be called otherwise
            let
                -- setting inital state
                firstSowingState =
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
            { game | board = { newBoard | sowingState = firstSowingState } }

        _ ->
            -- should never get to this point
            game


nextSowingStep : Game -> Game
nextSowingStep game =
    case game.board.sowingState of
        Sowing sowingInfo ->
            -- there are still seeds to sow
            let
                -- sow seed at current sowing position
                boardAfterSowing =
                    GameBoard.sowAtPosition game.board sowingInfo.position

                -- next iteration there is one less seed to sow
                seedsToSowNext =
                    sowingInfo.seedsToSow - 1
            in
            if seedsToSowNext == 0 then
                -- just sowed the last seed
                case sowingInfo.position of
                    RowPos player posInRow ->
                        if
                            player
                                == sowingInfo.playerSowing
                                && GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer boardAfterSowing player) posInRow
                                == 1
                            -- last seed sown to empty house in own row
                        then
                            -- needs to be handled specifically in next iteration
                            { game
                                | board = { boardAfterSowing | sowingState = HandleLastSeedInEmptyHouse player posInRow }
                            }

                        else
                            -- last seed sown to not-empty house or in other player's row
                            { game
                                | board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing False }
                            }

                    StorePos player ->
                        -- last seed sown to store: player gets one more turn
                        { game | board = { boardAfterSowing | sowingState = SowingFinished sowingInfo.playerSowing True } }

            else
                -- seed was not the last one: sowing needs to go on, next iteration at next position
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

        SowingFinished player oneMoreTurn ->
            -- last seed was sown in last iteration
            let
                lastSowingPlayersRow =
                    GameBoard.getRowForPlayer game.board player

                otherPlayersRow =
                    GameBoard.getRowForPlayer game.board (Player.togglePlayer player)
            in
            if GameBoard.isRowEmpty lastSowingPlayersRow || GameBoard.isRowEmpty otherPlayersRow then
                -- one row is empty, game ends
                { game | board = GameBoard.resetSowingStates game.board, state = End (findWinner game) }

            else if oneMoreTurn then
                -- player ended sowing in store, gets one more turn
                { game | board = GameBoard.resetSowingStates game.board }

            else
                -- other player gets turn
                { game | board = GameBoard.resetSowingStates game.board, state = Turn (Player.togglePlayer player) }

        HandleLastSeedInEmptyHouse player pos ->
            -- last seed was sown to an empty house in player's own row
            let
                boardAfter =
                    GameBoard.handleSeedInEmptyHouse game.board game.settings player pos
            in
            { game
                | board = { boardAfter | sowingState = SowingFinished player False }
            }

        _ ->
            -- should never get to this point
            game
