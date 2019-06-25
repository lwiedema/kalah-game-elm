module Datatypes exposing (Model, Msg(..), SettingOption(..))

import Game exposing (Game)
import Localization exposing (Language(..))
import Player exposing (Player(..))
import Settings exposing (Intelligence(..), SowingSpeed(..))


type alias Model =
    Game


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
