module KalahaAI exposing (nextMove, weightMoves)

import Datatypes exposing (Msg(..), SettingOption(..))
import Game exposing (Game)
import GameBoard
import Player exposing (Player(..))
import Random
import Settings exposing (Intelligence(..), Settings)
import Utils.ListHelper as ListHelper


randomnessRange : Intelligence -> Float
randomnessRange intelligence =
    case intelligence of
        High ->
            0.1

        Medium ->
            0.3

        Low ->
            0.5


weightMoves : Settings -> Random.Generator (List Float)
weightMoves settings =
    case settings.opponent of
        Settings.Computer intelligence ->
            -- get random weights for each possible move
            -- to avoid predictability and to model different levels of "intelligence"
            Random.list settings.numberOfHouses
                (Random.float
                    (1 - randomnessRange intelligence)
                    (1 + randomnessRange intelligence)
                )

        Settings.Real ->
            -- should never get here
            Random.list 0 (Random.float 0 0)


nextMove : Game -> List Float -> Maybe Int
nextMove game weights =
    -- return position of house to empty next
    let
        moves =
            List.indexedMap (\pos element -> element * moveQuality game pos) weights
    in
    case List.maximum moves of
        Just bestMoveQuality ->
            if bestMoveQuality == 0.0 then
                -- this point should never get reached
                -- "best" move was predicted as sowing from an empty house.
                -- it can only be the maximum if ALL houses are empty
                -- but game should be displayed as ended before that
                Nothing

            else
                Just (ListHelper.getIndex bestMoveQuality moves)

        Nothing ->
            -- this point should never get reached
            -- moves-Map was empty
            Nothing


moveQuality : Game -> Int -> Float
moveQuality game pos =
    let
        row =
            GameBoard.getRowForPlayer game.board Two

        opponentsRow =
            GameBoard.getRowForPlayer game.board One

        seeds =
            GameBoard.numberOfSeedsInHouse row pos
    in
    if seeds == 0 then
        -- cannot do this move
        0

    else if game.settings.numberOfHouses - pos == seeds then
        -- move would end in store
        3

    else if GameBoard.numberOfSeedsInHouse row (pos + seeds) == 0 then
        -- move would end in empty house
        -- the more seeds are in opposite house, the better it is
        2
            * (1
                + 0.1
                * toFloat
                    (GameBoard.numberOfSeedsInHouse
                        opponentsRow
                        (game.settings.numberOfHouses - (pos + seeds) - 1)
                    )
              )

    else if
        GameBoard.numberOfSeedsInHouse
            opponentsRow
            (game.settings.numberOfHouses - pos - 1)
            == 0
    then
        -- opposite house is empty, opponent could steal seeds at next move
        1 * (1 + 0.2 * toFloat seeds)

    else
        -- no "special" move, houses with more seeds are higher rated
        1 * (1 + 0.1 * toFloat seeds)
