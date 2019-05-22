module Lists exposing (any, elementAt, last, repeat, setElementAt)


repeat : Int -> a -> List a
repeat n x =
    if n > 0 then
        x :: repeat (n - 1) x

    else
        []


elementAt : List a -> Int -> Maybe a
elementAt list pos =
    -- get element at position starting at 0
    if pos < List.length list then
        last (List.take (pos + 1) list)

    else
        Nothing


setElementAt : List a -> Int -> a -> List a
setElementAt list pos element =
    if pos < List.length list then
        List.take pos list ++ element :: List.drop (pos + 1) list

    else
        list


last : List a -> Maybe a
last list =
    case list of
        l :: [] ->
            Just l

        _ :: ls ->
            last ls

        [] ->
            Nothing


any : (a -> Bool) -> List a -> Bool
any f list =
    case list of
        x :: xs ->
            f x || any f xs

        [] ->
            False
