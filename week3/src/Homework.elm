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

import Date exposing (fromIsoString, Date)

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
                Just x
        )
        xs


find : (a -> Bool) -> List a -> Maybe a
find f xss =
    case xss of
        [] ->
            Nothing

        head :: tail ->
            if f head then
                Just head

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
                Just ( key, value )
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
    case res of
        Ok r ->
            fb r

        Err l ->
            fa l


parseDate : Maybe String -> Maybe Date
parseDate v =
    Maybe.andThen (\x -> Result.toMaybe (Date.fromIsoString x)) v
