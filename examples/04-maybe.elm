module Main exposing (Model, Msg(..), Temperature(..), celsiusToFarehheit, farenheitToCelsius, init, main, swapTemperatureConverter, toTemperature, update, view, viewConverter)

import Browser
import Html exposing (Attribute, Html, button, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Temperature
    = Celsius Float
    | Farenheit Float
    | InvalidCelsiusInput String
    | InvalidFarenheitInput String


type alias Model =
    { temperature : Temperature
    }


init : Model
init =
    { temperature = Celsius 0 }



-- UPDATE


type Msg
    = ChangeCelsius String
    | ChangeFarenheit String
    | SwapTemperatureConverter


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeCelsius input ->
            { model | temperature = toTemperature input Celsius InvalidCelsiusInput }

        ChangeFarenheit input ->
            { model | temperature = toTemperature input Farenheit InvalidFarenheitInput }

        SwapTemperatureConverter ->
            { model | temperature = swapTemperatureConverter model.temperature }


toTemperature : String -> (Float -> Temperature) -> (String -> Temperature) -> Temperature
toTemperature input validTemp invalidTemp =
    case String.toFloat input of
        Just value ->
            validTemp value

        Nothing ->
            invalidTemp input


swapTemperatureConverter : Temperature -> Temperature
swapTemperatureConverter temperature =
    case temperature of
        Celsius value ->
            Farenheit (celsiusToFarehheit value)

        Farenheit value ->
            Celsius (farenheitToCelsius value)

        InvalidCelsiusInput badInput ->
            InvalidFarenheitInput badInput

        InvalidFarenheitInput badInput ->
            InvalidCelsiusInput badInput


celsiusToFarehheit : Float -> Float
celsiusToFarehheit tempInCelsius =
    tempInCelsius * 1.8 + 32


farenheitToCelsius : Float -> Float
farenheitToCelsius tempInFarenheit =
    (tempInFarenheit - 32) / 1.8



-- VIEW


view : Model -> Html Msg
view model =
    case model.temperature of
        Celsius value ->
            viewConverter ( "°C = ", String.fromFloat value ) "blue" ( "°F", String.fromFloat (celsiusToFarehheit value) ) ChangeCelsius

        Farenheit value ->
            viewConverter ( "°F = ", String.fromFloat value ) "blue" ( "°C", String.fromFloat (farenheitToCelsius value) ) ChangeFarenheit

        InvalidCelsiusInput badInput ->
            viewConverter ( "°C = ", badInput ) "red" ( "°F", "???" ) ChangeCelsius

        InvalidFarenheitInput badInput ->
            viewConverter ( "°F = ", badInput ) "red" ( "°C", "???" ) ChangeCelsius


viewConverter : ( String, String ) -> String -> ( String, String ) -> (String -> Msg) -> Html Msg
viewConverter temperature color equivalentTemp toMsg =
    span []
        [ input [ value (Tuple.second temperature), onInput toMsg, style "width" "40px", style "border-color" color ] []
        , text (Tuple.first temperature)
        , span [ style "color" color ] [ text (Tuple.second equivalentTemp) ]
        , text (Tuple.first equivalentTemp)
        , button [ onClick SwapTemperatureConverter ] [ text "swap" ]
        ]
