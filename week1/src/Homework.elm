module Homework exposing
    ( clap
    , compress
    , dropEvery
    , elementAt
    , isPalindrome
    , myButLast
    , myLast
    , myLength
    , myReverse
    )


myLast : List Int -> Maybe Int
myLast list =
    case list of
        [] ->
            Nothing

        [ a ] ->
            Just a

        _ :: tail ->
            myLast tail


myButLast : List Int -> Maybe Int
myButLast list =
    case list of
        [ a, _ ] ->
            Just a

        _ :: tail ->
            myButLast tail

        _ ->
            Nothing


elementAt : List Int -> Int -> Maybe Int
elementAt list k =
    case list of
        [] ->
            Nothing

        a :: tail ->
            if k == 1 then
                Just a

            else if k < 1 then
                Nothing

            else
                elementAt tail (k - 1)


myLength : List Int -> Int
myLength list =
    case list of
        [] ->
            0

        _ :: tail ->
            1 + myLength tail


myReverse : List a -> List a
myReverse list =
    case list of
        [] ->
            []

        a :: tail ->
            myReverse tail ++ List.singleton a


isPalindrome : List Int -> Bool
isPalindrome list =
    List.reverse list == list


compress : String -> String
compress str =
    case String.uncons str of
        Just ( c, s ) ->
            if String.startsWith (String.fromChar c) s then
                compress s

            else
                String.cons c (compress s)

        Nothing ->
            ""


dropEvery : String -> Int -> String
dropEvery str n =
    if String.length str < n then
        str

    else
        String.left (n - 1) str ++ dropEvery (String.dropLeft n str) n


clap : String -> String
clap str =
    String.join " 👏 " (String.words str)
