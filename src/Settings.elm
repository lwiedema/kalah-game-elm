module Settings exposing (Settings, SowingSpeed(..), defaultSettings, speedInMilliseconds)


type alias Settings =
    { numberOfHouses : Int
    , numberOfSeeds : Int
    , lastSeedsForFinishingPlayer : Bool
    , sowingSpeed : SowingSpeed
    }


type SowingSpeed
    = Slow
    | Normal
    | Fast


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
    , numberOfSeeds = 4
    , lastSeedsForFinishingPlayer = False
    , sowingSpeed = Normal
    }
