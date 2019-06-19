module Utils.ArrayHelper exposing (any, getWithDefault)

import Array exposing (Array)
import List


any : (a -> Bool) -> Array a -> Bool
any f array =
    List.any f (Array.toList array)


getWithDefault : Int -> Array a -> a -> a
getWithDefault pos array default =
    case Array.get pos array of
        Nothing ->
            default

        Just x ->
            x
