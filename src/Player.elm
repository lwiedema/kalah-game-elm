module Player exposing (Player(..), Winner(..), toString, togglePlayer)


type Player
    = One
    | Two


type Winner
    = Winner Player ( Int, Int )
      -- tuple with finalScore for player One and Two
    | Drawn


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        One ->
            Two

        Two ->
            One


toString : Player -> String
toString player =
    case player of
        One ->
            "1"

        Two ->
            "2"
