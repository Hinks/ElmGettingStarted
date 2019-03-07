import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : Int
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" 0 "" ""



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    
    Age agestr ->
      { model | age = String.toInt agestr |> Maybe.withDefault 0}

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "number" "Age" (String.fromInt model.age) Age 
    , viewInput "password" "Password" model.password Password 
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  passwordValidation model

passwordValidation : Model -> Html msg
passwordValidation model =
  if not (passwordIsOver 8 model.password) then
    passwordError "password is not long enough"
  else if not (containsRequiredVariationOfChars model.password) then 
    passwordError "password must contain upper case, lower case, and numeric characters."
  else if not (passwordMatch model.password model.passwordAgain) then
    passwordError "passwords do not match!"
  else
     div [ style "color" "green" ] [ text "OK" ]

passwordError : String -> Html msg
passwordError reasonWhy =
  div [ style "color" "red" ] [ text reasonWhy ]

passwordMatch : String -> String -> Bool
passwordMatch pw1 pw2 =
  pw1 == pw2

passwordIsOver : Int -> String -> Bool
passwordIsOver minLimit pw =
  if String.length pw < minLimit then False else True 

containsRequiredVariationOfChars : String -> Bool
containsRequiredVariationOfChars str =
  containsLowerCase str && containsUpperCase str && containsNumber str

containsLowerCase : String -> Bool
containsLowerCase str = 
  String.any Char.isLower str

containsUpperCase : String -> Bool
containsUpperCase str =
  String.any Char.isUpper str

containsNumber : String -> Bool
containsNumber str =
  String.any Char.isDigit str

{-- Try other solutions 
  1. Store character variations in a list
  2. Chain functions returning a Result
--}