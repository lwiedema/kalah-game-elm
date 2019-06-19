module Utils.ListHelper exposing (getIndex)

import List


getIndex : a -> List a -> Int
getIndex element list =
    case getIndexHelper element list 0 of
        Just index ->
            index

        Nothing ->
            0


getIndexHelper : a -> List a -> Int -> Maybe Int
getIndexHelper element list tmpIndex =
    case list of
        x :: xs ->
            if x == element then
                Just tmpIndex

            else
                getIndexHelper element xs (tmpIndex + 1)

        [] ->
            Nothing
