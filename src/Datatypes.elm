module Datatypes exposing (ErrorType(..), Model(..), Msg(..), SettingOption(..), errorCode, errorToString)

import Game exposing (Game)
import Localization exposing (Language(..))
import Player exposing (Player(..))
import Settings exposing (Intelligence(..), SowingSpeed(..))


type Model
    = GameOk Game
    | GameError ErrorType


type ErrorType
    = BestMoveZero
    | StartSowingIllegal
    | NextSowingStepIllegal
    | Unexpected


errorCode : ErrorType -> String
errorCode error =
    case error of
        BestMoveZero ->
            "BestMoveZero"

        StartSowingIllegal ->
            "StartSowingIllegal"

        NextSowingStepIllegal ->
            "NextSowingStepIllegal"

        Unexpected ->
            "Unexpected"


errorToString : ErrorType -> String
errorToString error =
    case error of
        BestMoveZero ->
            "There was no possible move found."

        StartSowingIllegal ->
            "Starting to sow not possible with current sowing status."

        NextSowingStepIllegal ->
            "No next step possible with current sowing status."

        Unexpected ->
            "An unexpected error occurred."


type Msg
    = Click Player Int
    | NextSowingStep
    | Restart
    | OpenSettings
    | SettingChanged SettingOption
    | ComputerHasTurn
    | RandomMoveWeights (List Float)
    | DoNothing


type SettingOption
    = Speed SowingSpeed
    | SeedNumber Int
    | LastSeedsBehaviour
    | UpsideDown
    | SowOpponentsStore
    | OpponentOption
    | IntelligenceOption Intelligence
    | StartingPlayer
    | LanguageSetting Language
