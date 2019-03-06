module Main exposing (Model, Msg(..), init, main, update, view, viewConverter)

import Browser
import Html exposing (Attribute, Html, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { inputC : String
    , inputF : String
    }


init : Model
init =
    { inputC = "", inputF = "" }



-- UPDATE


type Msg
    = Change String
    | ChangeFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newC ->
            case String.toFloat newC of
                Just c ->
                    { model | inputF = String.fromFloat (c * 1.8 + 32), inputC = newC }

                Nothing ->
                    { model | inputC = newC, inputF = "???" }

        ChangeFahrenheit newF ->
            case String.toFloat newF of
                Just f ->
                    { model | inputC = String.fromFloat ((f - 32) * 5 / 9.0), inputF = newF }

                Nothing ->
                    { model | inputC = "???", inputF = newF }



-- VIEW


view : Model -> Html Msg
view model =
    viewConverter model


viewConverter : Model -> Html Msg
viewConverter model =
    span []
        [ input [ value model.inputC, onInput Change, style "width" "40px" ] []
        , text "°C = "
        , input [ value model.inputF, onInput ChangeFahrenheit, style "width" "40px" ] []
        , text "°F"
        ]
