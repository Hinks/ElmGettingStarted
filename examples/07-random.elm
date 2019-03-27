import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Dict exposing (Dict)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { dieFace : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 6)
      )

    NewFace newFace ->
      ( Model newFace
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let 
    dieFaceUnicode = 
      case Dict.get model.dieFace dieFaceUnicodes of
          Just value -> 
            value
      
          Nothing -> 
            '\u{1F648}' -- Monkey icon
  in           
    div []
      [ h1 [style "font-size" "48px"] [ text (String.fromChar dieFaceUnicode) ]
      , button [ onClick Roll ] [ text "Roll" ]
      ]


dieFaceUnicodes : Dict Int Char
dieFaceUnicodes = 
  Dict.fromList 
    [ (1, '\u{2680}')
    , (2, '\u{2681}')
    , (3, '\u{2682}')
    , (4, '\u{2683}')
    , (5, '\u{2684}')
    , (6, '\u{2685}')]