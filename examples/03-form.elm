module Main exposing (Model, Msg(..), containsAllVariationsOf, hasLowerCase, hasNumber, hasUpperCase, init, main, passwordError, passwordIsOver, passwordMatch, passwordValidation, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : Int
    , password : String
    , passwordAgain : String
    , isFinishedWithForm : Bool
    }


init : Model
init =
    Model "" 0 "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name, isFinishedWithForm = False }

        Age agestr ->
            { model | age = String.toInt agestr |> Maybe.withDefault 0, isFinishedWithForm = False }

        Password password ->
            { model | password = password, isFinishedWithForm = False }

        PasswordAgain password ->
            { model | passwordAgain = password, isFinishedWithForm = False }

        Submit ->
            { model | isFinishedWithForm = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "number" "Age" (String.fromInt model.age) Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.isFinishedWithForm then
        passwordValidation model

    else
        div [] [ text "Fill in the form." ]


passwordValidation : Model -> Html msg
passwordValidation model =
    if not (passwordIsOver 8 model.password) then
        passwordError "password is not long enough"

    else if not (containsAllVariationsOf [ hasLowerCase, hasUpperCase, hasNumber ] model.password) then
        passwordError "password must contain upper case, lower case, and numeric characters."

    else if not (passwordMatch model.password model.passwordAgain) then
        passwordError "passwords do not match!"

    else
        div [ style "color" "green" ] [ text "Ok" ]


passwordError : String -> Html msg
passwordError reasonWhy =
    div [ style "color" "red" ] [ text reasonWhy ]



-- PASSWORD VALIDATORS


passwordMatch : String -> String -> Bool
passwordMatch pw1 pw2 =
    pw1 == pw2


passwordIsOver : Int -> String -> Bool
passwordIsOver minLimit pw =
    if String.length pw < minLimit then
        False

    else
        True


containsAllVariationsOf : List (String -> Bool) -> String -> Bool
containsAllVariationsOf charValidators password =
    let
        validations =
            List.map (\validator -> validator password) charValidators
    in
    List.all (\validation -> validation == True) validations


hasLowerCase : String -> Bool
hasLowerCase str =
    String.any Char.isLower str


hasUpperCase : String -> Bool
hasUpperCase str =
    String.any Char.isUpper str


hasNumber : String -> Bool
hasNumber str =
    String.any Char.isDigit str



{--Try other solutions 
  1. Store character variations in a list
  2. Chain functions returning a Result
--}
