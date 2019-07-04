module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MyList exposing (MyList(..))
import Test exposing (..)


list123 : MyList Int
list123 =
    Node 1 (Node 2 (Node 3 Empty))


list321 : MyList Int
list321 =
    Node 3 (Node 2 (Node 1 Empty))


double : Int -> Int
double num =
    num * 2


suite : Test
suite =
    Test.concat [ testCons, testSum, testLength, testReverse, testMap, testFoldl ]


testCons : Test
testCons =
    describe "cons"
        [ test "element with empty list should result in one element list" <|
            \_ ->
                Empty
                    |> MyList.cons 1
                    |> Expect.equal (Node 1 Empty)
        , test "with non empty list should give new list with element as the head" <|
            \_ ->
                Node 3 (Node 2 (Node 1 Empty))
                    |> MyList.cons 4
                    |> Expect.equal (Node 4 (Node 3 (Node 2 (Node 1 Empty))))
        ]


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
                list123
                    |> MyList.length
                    |> Expect.equal 3
        ]



-- REVERSE


testReverse : Test
testReverse =
    describe "Reverse MyList"
        [ test "revese on an empty list should not have any effect" <|
            \_ ->
                Empty
                    |> MyList.reverse
                    |> Expect.equal Empty
        , test "reverse on a non empty list should reverse the elements" <|
            \_ ->
                list123
                    |> MyList.reverse
                    |> Expect.equal list321
        ]



-- SUM


testSum : Test
testSum =
    describe "MyList summing ints"
        [ test "Recursive implementation of sum" <|
            \_ ->
                list123
                    |> MyList.sum
                    |> Expect.equal 6
        , test "Tail call implementation of sum" <|
            \_ ->
                list123
                    |> MyList.sumTailRec
                    |> Expect.equal 6
        ]



-- MAP


testMap : Test
testMap =
    describe "List map test"
        [ test "map recursive" <|
            \_ ->
                list123
                    |> MyList.map (\x -> x * 2)
                    |> Expect.equal (Node 2 (Node 4 (Node 6 Empty)))
        , test "map tail recursive" <|
            \_ ->
                list123
                    |> MyList.mapTailRec double
                    |> Expect.equal (Node 2 (Node 4 (Node 6 Empty)))
        ]



--FOLR


testFoldl : Test
testFoldl =
    describe "List foldl test"
        [ test "summing list of ints" <|
            \_ ->
                list123
                    |> MyList.foldl (\next acc -> next + acc) 0
                    |> Expect.equal 6
        , test "foldl passing cons function" <|
            \_ ->
                list123
                    |> MyList.foldl MyList.cons Empty
                    |> Expect.equal (Node 3 (Node 2 (Node 1 Empty)))
        ]
