module KalahaAI exposing (nextMove, weightMoves)

import Datatypes exposing (Model, Msg(..), SettingOption(..))
import GameBoard
import Player exposing (Player(..))
import Random
import Settings exposing (Settings)
import Utils.ListHelper as ListHelper


weightMoves : Settings -> Random.Generator (List Float)
weightMoves settings =
    case settings.opponent of
        Settings.Computer intelligence ->
            -- get random weights for each possible move
            -- to avoid predictability and to model different levels of "intelligence"
            Random.list settings.numberOfHouses
                (Random.float
                    (1 - Settings.randomnessRange intelligence)
                    (1 + Settings.randomnessRange intelligence)
                )

        Settings.Real ->
            -- should never get here
            Random.list 0 (Random.float 0 0)


nextMove : Model -> List Float -> Int
nextMove model weights =
    -- return position of house to empty next
    let
        moves =
            List.indexedMap (\pos element -> element * moveQuality model pos) weights
    in
    case List.maximum moves of
        Just bestMoveQuality ->
            ListHelper.getIndex bestMoveQuality moves

        Nothing ->
            0


moveQuality : Model -> Int -> Float
moveQuality model pos =
    let
        row =
            GameBoard.getRowForPlayer model.board Two

        opponentsRow =
            GameBoard.getRowForPlayer model.board One

        seeds =
            GameBoard.numberOfSeedsInHouse row pos
    in
    if seeds == 0 then
        -- cannot do this move
        0

    else if model.settings.numberOfHouses - pos == seeds then
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
                        (model.settings.numberOfHouses - (pos + seeds) - 1)
                    )
              )

    else if
        GameBoard.numberOfSeedsInHouse
            opponentsRow
            (model.settings.numberOfHouses - pos - 1)
            == 0
    then
        -- opposite house is empty, opponent could steal seeds at next move
        1 * (1 + 0.2 * toFloat seeds)

    else
        -- no "special" move, houses with more seeds are higher rated
        1 * (1 + 0.1 * toFloat seeds)
