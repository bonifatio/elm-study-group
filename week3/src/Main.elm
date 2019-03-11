module Homework exposing
    ( either
    , find
    , keepOks
    , mapOk
    , maybeToList
    , parseDate
    , updateList
    , updateListKv
    )

import Date exposing (Date)


maybeToList : Maybe a -> List a
maybeToList x =
    Maybe.withDefault [] (Maybe.map List.singleton x)


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f xs =
    List.filterMap
        (\x ->
            if shouldChange x then
                f x

            else
                Nothing
        )
        xs


find : (a -> Bool) -> List a -> Maybe a
find f xss =
    case xss of
        [] ->
            Just False

        head :: tail ->
            if f head then
                Just True

            else
                find f tail


updateListKv :
    List ( k, v )
    -> k
    -> (v -> Maybe v)
    -> List ( k, v )
updateListKv old k f =
    List.filterMap
        (\( key, value ) ->
            if key == k then
                Maybe.map (\x -> ( key, x )) (f value)

            else
                Just ( k, value )
        )
        old


keepOks : List (Result a b) -> List b
keepOks xss =
    List.filterMap Result.toMaybe xss


mapOk : (b -> c) -> Result a b -> Result a c
mapOk f res =
    Result.map f res


either : (a -> c) -> (b -> c) -> Result a b -> c
either fa fb res =
    Result.mapError fa (Result.map fb res)


parseDate : Maybe String -> Maybe Date
parseDate v =
    Debug.todo ""
