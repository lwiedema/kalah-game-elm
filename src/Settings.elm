module Settings exposing (Intelligence(..), Opponent(..), Settings, SowingSpeed(..), defaultSettings, speedInMilliseconds, toggleOpponentOption)

import Localization


type alias Settings =
    { numberOfHouses : Int
    , settingsOpen : Bool
    , numberOfSeeds : Int
    , lastSeedsForFinishingPlayer : Bool
    , sowingSpeed : SowingSpeed
    , upsideDownEnabled : Bool
    , sowInOpponentsStore : Bool
    , playerTwoStarting : Bool
    , opponent : Opponent
    , language : Localization.Language
    }


type SowingSpeed
    = Slow
    | Normal
    | Fast


type Opponent
    = Real
    | Computer Intelligence


type Intelligence
    = High
    | Medium
    | Low


toggleOpponentOption : Settings -> Settings
toggleOpponentOption settings =
    case settings.opponent of
        Real ->
            { settings | opponent = Computer Medium }

        Computer _ ->
            { settings | opponent = Real }


speedInMilliseconds : SowingSpeed -> Float
speedInMilliseconds sowingSpeed =
    case sowingSpeed of
        Slow ->
            800

        Normal ->
            500

        Fast ->
            200


defaultSettings : Settings
defaultSettings =
    { numberOfHouses = 6
    , settingsOpen = False
    , numberOfSeeds = 4
    , lastSeedsForFinishingPlayer = False
    , sowingSpeed = Normal
    , upsideDownEnabled = False
    , sowInOpponentsStore = False
    , opponent = Real
    , playerTwoStarting = False
    , language = Localization.German
    }
