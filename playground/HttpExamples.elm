module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List String


init : Model
init =
    []



-- UPDATE


update : msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            []



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ text "hello"
        ]
