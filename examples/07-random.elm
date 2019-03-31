import Browser
import Random
import Dict exposing (Dict)
import Html
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
  ( Model 6
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


view : Model -> Html.Html Msg
view model =
  Html.div []
  [
    svg [ width "120", height "120", viewBox "0 0 120 120" ] (drawDice model)
    , Html.div [] []
    , Html.button [Html.Events.onClick Roll] [Html.text "Roll"]


  ]
  
drawDice : Model -> List (Svg.Svg msg)
drawDice model =
    List.concat [ [roundRect], (drawDieFace model.dieFace) ]

drawDieFace : Int -> List (Svg.Svg msg)
drawDieFace dieFace = 
  case dieFace of
      1 ->
          [ circle [ cx "60", cy "60", r "10" ] [] ]
      2 ->
          [ 
            circle [ cx "45", cy "60", r "10" ] []
          , circle [ cx "75", cy "60", r "10" ] [] 
          ]
      3 ->
          [ 
            circle [ cx "30", cy "90", r "10" ] []
          , circle [ cx "60", cy "60", r "10" ] []
          , circle [ cx "90", cy "30", r "10" ] [] 
          ]

      4 -> 
          [ 
            circle [ cx "40", cy "40", r "10" ] [] 
          , circle [ cx "80", cy "40", r "10" ] []
          , circle [ cx "40", cy "80", r "10" ] []
          , circle [ cx "80", cy "80", r "10" ] []
          ]
      5 -> 
          [ 
            circle [ cx "40", cy "40", r "10" ] [] 
          , circle [ cx "80", cy "40", r "10" ] []
          , circle [ cx "60", cy "60", r "10" ] []
          , circle [ cx "40", cy "80", r "10" ] []
          , circle [ cx "80", cy "80", r "10" ] []
          ]
      _ ->
          [ 
            circle [ cx "45", cy "30", r "10" ] [] 
          , circle [ cx "45", cy "60", r "10" ] [] 
          , circle [ cx "45", cy "90", r "10" ] [] 
          , circle [ cx "75", cy "30", r "10" ] [] 
          , circle [ cx "75", cy "60", r "10" ] [] 
          , circle [ cx "75", cy "90", r "10" ] [] 
          ]         



roundRect : Svg.Svg msg
roundRect =
  rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", roundRectStyle] [] 

roundRectStyle : Svg.Attribute msg
roundRectStyle = 
  Svg.Attributes.style "fill:lightgrey;stroke:black;stroke-width:2;opacity:0.5"