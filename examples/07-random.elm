module Main exposing (Face(..), Model, Msg(..), drawDice, drawDie, drawDieFace, init, main, roundRect, roundRectStyle, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html
import Html.Events
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



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


type alias Die =
    { score : Face
    , flips : List Face
    }


type alias Model =
    { dice : List Die
    }


initialDie : Die
initialDie =
    Die One []


getDieFace : Die -> Face
getDieFace die =
    case List.head die.flips of
        Nothing ->
            die.score

        Just face ->
            face


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ initialDie, initialDie, initialDie ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | DiceState (List Die)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate DiceState (randomDice (List.length model.dice))
            )

        DiceState newState ->
            ( { model | dice = newState }
            , Cmd.none
            )



-- GENERATING RANDOM DICE


randomDice : Int -> Random.Generator (List Die)
randomDice number =
    Random.list number randomDie


randomDie : Random.Generator Die
randomDie =
    let
        score =
            randomFace

        flips =
            randomFlips 5
    in
    Random.map2 Die score flips


randomFlips : Int -> Random.Generator (List Face)
randomFlips number =
    Random.list number randomFace


randomFace : Random.Generator Face
randomFace =
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
    if allDiceAreStill model.dice then
        Sub.none

    else
        Time.every 500 (\_ -> DiceState <| flipDice model.dice)


flipDice : List Die -> List Die
flipDice dice =
    List.map flipDie dice


allDiceAreStill : List Die -> Bool
allDiceAreStill dice =
    List.all isDieStill dice


isDieStill : Die -> Bool
isDieStill die =
    List.isEmpty die.flips


flipDie : Die -> Die
flipDie die =
    case List.length die.flips of
        0 ->
            die

        _ ->
            { die | flips = List.drop 1 die.flips }



-- VIEW


type alias DieWithOrderNr =
    { orderNr : Int
    , die : Die
    }


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
        |> List.indexedMap DieWithOrderNr
        |> List.map drawDie
        |> List.concat


drawDie : DieWithOrderNr -> List (Svg.Svg msg)
drawDie dieWithOrderNr =
    List.concat
        [ [ roundRect dieWithOrderNr.orderNr ]
        , drawDieFace (DieFaceDrawAttr dieWithOrderNr.orderNr (getDieFace dieWithOrderNr.die))
        ]


roundRect : Int -> Svg.Svg msg
roundRect orderNr =
    rect [ x (String.fromInt (100 * orderNr)), y "0", width "100", height "100", rx "15", ry "15", roundRectStyle ] []


roundRectStyle : Svg.Attribute msg
roundRectStyle =
    Svg.Attributes.style "fill:lightgrey;stroke:black;stroke-width:2;opacity:0.5"


type alias DieFaceDrawAttr =
    { rectOrderNr : Int
    , dieFace : Face
    }


drawDieFace : DieFaceDrawAttr -> List (Svg.Svg msg)
drawDieFace dieFaceDrawAttr =
    let
        dotRadius =
            10

        rectCenterX =
            (100 * dieFaceDrawAttr.rectOrderNr) + 50
    in
    case dieFaceDrawAttr.dieFace of
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
