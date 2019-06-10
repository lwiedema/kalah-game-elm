module Settings exposing (Settings, defaultSettings)


type alias Settings =
    { numberOfHouses : Int
    , numberOfSeeds : Int
    , lastSeedsForFinishingPlayer : Bool
    }


defaultSettings : Settings
defaultSettings =
    { numberOfHouses = 6
    , numberOfSeeds = 4
    , lastSeedsForFinishingPlayer = False
    }
