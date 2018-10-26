module SafeUtils exposing (findSafeInDict, findSafeInList)

import Array exposing (Array)


findSafeInDict : Int -> Array a -> a
findSafeInDict id dict =
    case Array.get id dict of
        Just res ->
            res

        Nothing ->
            Debug.todo ("Didn't find " ++ String.fromInt id ++ " in " ++ Debug.toString dict)


findSafeInList : Int -> List a -> a
findSafeInList id list =
    case Array.fromList list |> Array.get id of
        Just el ->
            el

        Nothing ->
            Debug.todo ("Didn't find " ++ String.fromInt id ++ " in " ++ Debug.toString list)
