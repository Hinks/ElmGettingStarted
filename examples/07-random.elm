module Main exposing (Face(..), Model, Msg(..), drawDice, drawDieFace, drawSingleDice, init, main, roll, rolls, roundRect, roundRectStyle, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html
import Html.Events
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



--


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type alias Model =
    { dice : List Face
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ One, Two, Three ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | RandomDiceFlips (List Face)
    | NewFaces (List Face)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFaces (rolls (List.length model.dice))
            )

        RandomDiceFlips diceFlips ->
            ( model
            , Cmd.none
            )

        NewFaces faces ->
            ( Model faces
            , Cmd.none
            )


rolls : Int -> Random.Generator (List Face)
rolls nrOfDice =
    Random.list nrOfDice roll


roll : Random.Generator Face
roll =
    Random.weighted
        ( 40, One )
        [ ( 20, Two )
        , ( 10, Three )
        , ( 10, Four )
        , ( 10, Five )
        , ( 10, Six )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ svg [ width "120", height "120", viewBox "0 0 360 120" ] (drawDice model)
        , Html.div [] []
        , Html.button [ Html.Events.onClick Roll ] [ Html.text "Roll" ]
        ]


drawDice : Model -> List (Svg.Svg msg)
drawDice model =
    model.dice
    |> List.indexedMap Tuple.pair 
    |> List.map drawSingleDice
    |> List.concat


drawSingleDice : ( Int, Face ) -> List (Svg.Svg msg)
drawSingleDice diceWithIndex =
    List.concat
        [ [ roundRect (Tuple.first diceWithIndex) ]
        , drawDieFace diceWithIndex
        ]


drawDieFace : ( Int, Face ) -> List (Svg.Svg msg)
drawDieFace diceWithIndex =
    let
        index =
            Tuple.first diceWithIndex

        dotRadius =
            10

        rectCenterX =
            (100 * index) + 50
    in
    case Tuple.second diceWithIndex of
        One ->
            [ circle (dot rectCenterX 50 dotRadius) [] ]

        Two ->
            [ circle (dot (rectCenterX - 15) 50 dotRadius) []
            , circle (dot (rectCenterX + 15) 50 dotRadius) []
            ]

        Three ->
            [ circle (dot (rectCenterX - 30) 80 dotRadius) []
            , circle (dot rectCenterX 50 dotRadius) []
            , circle (dot (rectCenterX + 30) 20 dotRadius) []
            ]

        Four ->
            [ circle (dot (rectCenterX - 20) 30 dotRadius) []
            , circle (dot (rectCenterX + 20) 30 dotRadius) []
            , circle (dot (rectCenterX - 20) 70 dotRadius) []
            , circle (dot (rectCenterX + 20) 70 dotRadius) []
            ]

        Five ->
            [ circle (dot (rectCenterX - 20) 30 dotRadius) []
            , circle (dot (rectCenterX + 20) 30 dotRadius) []
            , circle (dot rectCenterX 50 dotRadius) []
            , circle (dot (rectCenterX - 20) 70 dotRadius) []
            , circle (dot (rectCenterX + 20) 70 dotRadius) []
            ]

        Six ->
            [ circle (dot (rectCenterX - 15) 20 dotRadius) []
            , circle (dot (rectCenterX - 15) 50 dotRadius) []
            , circle (dot (rectCenterX - 15) 80 dotRadius) []
            , circle (dot (rectCenterX + 15) 20 dotRadius) []
            , circle (dot (rectCenterX + 15) 50 dotRadius) []
            , circle (dot (rectCenterX + 15) 80 dotRadius) []
            ]


dot : Int -> Int -> Int -> List (Svg.Attribute msg)
dot cx cy radius =
    [ Svg.Attributes.cx (String.fromInt cx)
    , Svg.Attributes.cy (String.fromInt cy)
    , Svg.Attributes.r (String.fromInt radius)
    ]


roundRect : Int -> Svg.Svg msg
roundRect diceNrX =
    rect [ x (String.fromInt (100 * diceNrX)), y "0", width "100", height "100", rx "15", ry "15", roundRectStyle ] []


roundRectStyle : Svg.Attribute msg
roundRectStyle =
    Svg.Attributes.style "fill:lightgrey;stroke:black;stroke-width:2;opacity:0.5"
