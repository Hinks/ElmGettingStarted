module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MyList exposing (MyList(..))
import Test exposing (..)


listOfNumbers : MyList Int
listOfNumbers =
    Node 1 (Node 2 (Node 3 Empty))


listOfNumbersReversed : MyList Int
listOfNumbersReversed =
    Node 3 (Node 2 (Node 1 Empty))


suite : Test
suite =
    Test.concat [ testSum, testLength, testReverse ]



-- CONS
-- HEAD
-- TAIL
-- LENGTH


testLength : Test
testLength =
    describe "MyList length"
        [ test "The length of an empty list should be zero" <|
            \_ ->
                Empty
                    |> MyList.length
                    |> Expect.equal 0
        , test "The length on an non empty list should be equal to the number of elements in the list" <|
            \_ ->
                listOfNumbers
                    |> MyList.length
                    |> Expect.equal 3
        ]



-- REVERSE


testReverse : Test
testReverse =
    describe "Reverse MyList"
        [ test "revese empty list should just return empty list" <|
            \_ ->
                Empty
                    |> MyList.reverse
                    |> Expect.equal Empty
        , test "reverse non empty list should reverse the elements" <|
            \_ ->
                listOfNumbers
                    |> MyList.reverse
                    |> Expect.equal listOfNumbersReversed
        ]



-- SUM


testSum : Test
testSum =
    describe "MyList summing ints"
        [ test "Recursive implementation of sum" <|
            \_ ->
                listOfNumbers
                    |> MyList.sum
                    |> Expect.equal 6
        , test "Tail call implementation of sum" <|
            \_ ->
                listOfNumbers
                    |> MyList.sumTailRec
                    |> Expect.equal 6
        ]
