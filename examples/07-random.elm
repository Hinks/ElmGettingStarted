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

type Face = One | Two | Three | Four | Five | Six

type alias Model =
  { dieFace : Face
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model One
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Face


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace roll
      )

    NewFace face ->
      ( Model face
      , Cmd.none
      )


roll : Random.Generator Face
roll =
  Random.weighted
    (40, One)
    [ (20, Two)
    , (10, Three)
    , (10, Four)
    , (10, Five)
    , (10, Six)
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
  let 
    size = 
      "120"
  in
    Html.div []
    [
      svg [ width size, height size, viewBox "0 0 120 120" ] (drawDice model)
      , Html.div [] []
      , Html.button [Html.Events.onClick Roll] [Html.text "Roll"]
    ]
  
drawDice : Model -> List (Svg.Svg msg)
drawDice model =
    List.concat [ [roundRect], (drawDieFace model.dieFace) ]

drawDieFace : Face -> List (Svg.Svg msg)
drawDieFace dieFace = 
  case dieFace of
      One ->
          [ circle [ cx "60", cy "60", r "10" ] [] ]
      Two ->
          [ 
            circle [ cx "45", cy "60", r "10" ] []
          , circle [ cx "75", cy "60", r "10" ] [] 
          ]
      Three ->
          [ 
            circle [ cx "30", cy "90", r "10" ] []
          , circle [ cx "60", cy "60", r "10" ] []
          , circle [ cx "90", cy "30", r "10" ] [] 
          ]

      Four -> 
          [ 
            circle [ cx "40", cy "40", r "10" ] [] 
          , circle [ cx "80", cy "40", r "10" ] []
          , circle [ cx "40", cy "80", r "10" ] []
          , circle [ cx "80", cy "80", r "10" ] []
          ]
      Five -> 
          [ 
            circle [ cx "40", cy "40", r "10" ] [] 
          , circle [ cx "80", cy "40", r "10" ] []
          , circle [ cx "60", cy "60", r "10" ] []
          , circle [ cx "40", cy "80", r "10" ] []
          , circle [ cx "80", cy "80", r "10" ] []
          ]
      Six ->
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