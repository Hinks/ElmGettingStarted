module Main exposing (Face(..), Model, Msg(..), init, main, roundRectStyle, subscriptions, update, view, viewDice, viewDie, viewDieFace, viewRoundRect)

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


type alias DieViewAttribute =
    { orderNr : Int
    , die : Die
    , size : Int
    }


toDieViewAttribute : Int -> Int -> Die -> DieViewAttribute
toDieViewAttribute size orderNr die =
    DieViewAttribute orderNr die size


view : Model -> Html.Html Msg
view model =
    let
        dieSize =
            120

        viewBoxWidth =
            String.fromInt (dieSize * List.length model.dice)

        viewBoxHight =
            "120"

        viewBoxSize =
            "0 0 " ++ viewBoxWidth ++ " " ++ viewBoxHight

        theWidth =
            String.fromInt (dieSize + (dieSize // 5))

        theHeight =
            theWidth
    in
    Html.div []
        [ svg [ width theWidth, height theHeight, viewBox viewBoxSize ] (viewDice dieSize model)
        , Html.div [] []
        , Html.button [ Html.Events.onClick Roll ] [ Html.text "Roll" ]
        ]


viewDice : Int -> Model -> List (Svg.Svg msg)
viewDice dieSize model =
    model.dice
        |> List.indexedMap (toDieViewAttribute dieSize)
        |> List.map viewDie
        |> List.concat


viewDie : DieViewAttribute -> List (Svg.Svg msg)
viewDie dieViewInfo =
    List.concat
        [ [ viewRoundRect dieViewInfo ]
        , viewDieFace dieViewInfo
        ]


viewRoundRect : DieViewAttribute -> Svg.Svg msg
viewRoundRect dieViewInfo =
    let
        theWidth =
            String.fromInt dieViewInfo.size

        theHeight =
            theWidth

        xpos =
            String.fromInt (dieViewInfo.size * dieViewInfo.orderNr)
    in
    rect [ x xpos, y "0", width theWidth, height theHeight, rx "15", ry "15", roundRectStyle ] []


roundRectStyle : Svg.Attribute msg
roundRectStyle =
    Svg.Attributes.style "fill:lightgrey;stroke:black;stroke-width:2;opacity:0.5"


viewDieFace : DieViewAttribute -> List (Svg.Svg msg)
viewDieFace dieViewAttr =
    case getDieFace dieViewAttr.die of
        One ->
            [ circle (viewDot center dieViewAttr) [] ]

        Two ->
            [ circle (viewDot (Point 4 6) dieViewAttr) []
            , circle (viewDot (Point 8 6) dieViewAttr) []
            ]

        Three ->
            [ circle (viewDot bottomLeft dieViewAttr) []
            , circle (viewDot center dieViewAttr) []
            , circle (viewDot upperRight dieViewAttr) []
            ]

        Four ->
            [ circle (viewDot upperLeft dieViewAttr) []
            , circle (viewDot upperRight dieViewAttr) []
            , circle (viewDot bottomLeft dieViewAttr) []
            , circle (viewDot bottomRight dieViewAttr) []
            ]

        Five ->
            [ circle (viewDot upperLeft dieViewAttr) []
            , circle (viewDot upperRight dieViewAttr) []
            , circle (viewDot center dieViewAttr) []
            , circle (viewDot bottomLeft dieViewAttr) []
            , circle (viewDot bottomRight dieViewAttr) []
            ]

        Six ->
            [ circle (viewDot (Point 4 3) dieViewAttr) []
            , circle (viewDot (Point 4 6) dieViewAttr) []
            , circle (viewDot (Point 4 9) dieViewAttr) []
            , circle (viewDot (Point 8 3) dieViewAttr) []
            , circle (viewDot (Point 8 6) dieViewAttr) []
            , circle (viewDot (Point 8 9) dieViewAttr) []
            ]


type alias Point =
    { x : Int
    , y : Int
    }


viewDot : Point -> DieViewAttribute -> List (Svg.Attribute msg)
viewDot point die =
    let
        radius =
            die.size // 10

        rectTopLeftCornerPosX =
            die.size * die.orderNr

        gridValue =
            die.size // 12

        spotInRectGrid =
            ( rectTopLeftCornerPosX + point.x * gridValue, point.y * gridValue )
    in
    [ Svg.Attributes.cx <| String.fromInt <| Tuple.first spotInRectGrid
    , Svg.Attributes.cy <| String.fromInt <| Tuple.second spotInRectGrid
    , Svg.Attributes.r <| String.fromInt <| radius
    ]


upperLeft : Point
upperLeft =
    Point 2 2


upperRight : Point
upperRight =
    Point 10 2


center : Point
center =
    Point 6 6


bottomLeft : Point
bottomLeft =
    Point 2 10


bottomRight : Point
bottomRight =
    Point 10 10
