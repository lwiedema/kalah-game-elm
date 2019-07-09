module Datatypes exposing (ErrorType(..), Model(..), Msg(..), SettingOption(..), errorToString)

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


errorToString : ErrorType -> String
errorToString error =
    case error of
        BestMoveZero ->
            "BestMoveZero"

        StartSowingIllegal ->
            "StartSowingIllegal"

        NextSowingStepIllegal ->
            "StartSowingIllegal"

        Unexpected ->
            "Unexpected"


type Msg
    = Click Player Int
    | NextSowingStep
    | Restart
    | OpenSettings
    | SettingChanged SettingOption
    | ComputerHasTurn
    | RandomMoveWeights (List Float)


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
