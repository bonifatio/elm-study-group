module Homework exposing
    ( bird
    , bird2
    , bird3
    , buildStatsUrl
    , catMaybes
    , convert
    , convert02
    , convert03
    , mapMaybes
    , setPhone
    )


import Url.Builder exposing (string)


convert :
    List { name : String, email : String, phone_number : String }
    -> List { name : String, email : String }
convert =
    List.map (\r -> { name = r.name, email = r.email })


convert02 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert02 =
    List.filterMap
        (\r ->
            case ( r.name, r.email ) of
                ( Just n, Just e ) ->
                    Just { name = n, email = e }

                _ ->
                    Nothing
        )


convert03 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert03 =
    List.filterMap
        (\r ->
            case r.name of
                Just n ->
                    let
                        eml =
                            case r.email of
                                Just e ->
                                    e

                                _ ->
                                    "<unspecified>"
                    in
                    Just { name = n, email = eml }

                _ ->
                    Nothing
        )


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))



-- using <|


bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr [ 1, 2, 3 ]



-- using |>


bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
        |> List.map incr
        |> List.filter notThree
        |> List.sum


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone p user =
    { user | profile = { address = { phone = p } } }


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes =
    List.filterMap


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
    let
        nElements =
            Maybe.map (\n -> string "start_date" (String.fromInt n)) ps.numElems

        stDate =
            Maybe.map (\d -> string "start_date" d) ps.startDate

        params =
            catMaybes [ nElements, stDate ]
    in
    Url.Builder.crossOrigin "https://myapi.com" [ "api", "item", String.fromInt itemId, "stats.json" ] params
