module Player exposing (Player(..), Winner(..), togglePlayer)


type Player
    = One
    | Two


type Winner
    = Winner Player
    | Drawn


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        One ->
            Two

        Two ->
            One
