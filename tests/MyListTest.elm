module MyListTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MyList exposing (MyList(..))
import Test exposing (..)


suite : Test
suite =
    Test.concat [ testCons, testSum, testLength, testReverse, testMap, testFoldl, testFoldr ]


testCons : Test
testCons =
    describe "MyList.cons"
        [ test "element with empty list should result in one element list" <|
            \_ ->
                Empty
                    |> MyList.cons 1
                    |> Expect.equal (Node 1 Empty)
        , test "element with non empty list should result in new list with element as the head" <|
            \_ ->
                let
                    list =
                        Node 3 (Node 2 (Node 1 Empty))

                    expectedList =
                        Node 4 (Node 3 (Node 2 (Node 1 Empty)))
                in
                list
                    |> MyList.cons 4
                    |> Expect.equal expectedList
        ]


testLength : Test
testLength =
    describe "MyList.length"
        [ test "On an empty list should be zero" <|
            \_ ->
                Empty
                    |> MyList.length
                    |> Expect.equal 0
        , test "On an non empty list should be equal to the number of elements" <|
            \_ ->
                let
                    list =
                        Node 1 (Node 2 (Node 3 Empty))
                in
                list
                    |> MyList.length
                    |> Expect.equal 3
        ]



-- REVERSE


testReverse : Test
testReverse =
    describe "MyList.reverse"
        [ test "On an empty list should not have any effect" <|
            \_ ->
                Empty
                    |> MyList.reverse
                    |> Expect.equal Empty
        , test "On a non empty list should reverse the elements" <|
            \_ ->
                let
                    list =
                        Node 1 (Node 2 (Node 3 Empty))

                    listReversed =
                        Node 3 (Node 2 (Node 1 Empty))
                in
                list
                    |> MyList.reverse
                    |> Expect.equal listReversed
        ]



-- SUM


testSum : Test
testSum =
    describe "MyList.sum"
        [ test "Recursive sum" <|
            \_ ->
                Node 1 (Node 2 (Node 3 Empty))
                    |> MyList.sum
                    |> Expect.equal 6
        , test "Tail recursive" <|
            \_ ->
                Node 1 (Node 2 (Node 3 Empty))
                    |> MyList.sumTailRec
                    |> Expect.equal 6
        ]



-- MAP


testMap : Test
testMap =
    let
        double =
            \x -> x * 2
    in
    describe "MyList.map"
        [ test "Recursive" <|
            \_ ->
                Node 1 (Node 2 (Node 3 Empty))
                    |> MyList.map double
                    |> Expect.equal (Node 2 (Node 4 (Node 6 Empty)))
        , test "Tail recursive" <|
            \_ ->
                Node 1 (Node 2 (Node 3 Empty))
                    |> MyList.mapTailRec double
                    |> Expect.equal (Node 2 (Node 4 (Node 6 Empty)))
        ]



-- FOLDL
-- f(3, f(2, f(1, z)))

-- foldl cons | [] [1[2[3[]]]] 
-- foldl cons | [1[]] [2[3[]]] 
-- foldl cons | [2[1[]]] [3[]]
-- foldl cons | [3[2[1[]]]] []
-- [3[2[1[]]]]

testFoldl : Test
testFoldl =
    describe "MyList.foldl"
        [ test "Foldl over list with cons should return the reversed version of the list" <|
            \_ ->
                let
                    list =
                        Node 1 (Node 2 (Node 3 Empty))

                    listAfterFoldl =
                        Node 3 (Node 2 (Node 1 Empty))
                in
                list
                    |> MyList.foldl MyList.cons Empty
                    |> Expect.equal listAfterFoldl
        ]



-- FOLDR
-- f(1, f(2, f(3, z)))

-- foldr cons | [] [1[2[3[]]]]
-- foldr cons | cons 1 foldr cons [] [2[3[]]]
-- foldr cons | cons 1 cons 2 foldr cons [] [3[]]
-- foldr cons | cons 1 cons 2 cons 3 foldr cons [] []
-- foldr cons | cons 1 cons 2 cons 3 []
-- foldr cons | cons 1 cons 2 [3[]]
-- foldr cons | cons 1 [2[3[]]]
-- [1[2[3[]]]]

testFoldr : Test
testFoldr =
    describe "MyList.foldr"
        [ test "Foldr over list with cons should return identical list" <|
            \_ ->
                let
                    list =
                        Node 1 (Node 2 (Node 3 Empty))

                    listAfterFoldr =
                        Node 1 (Node 2 (Node 3 Empty))
                in
                list
                    |> MyList.foldr MyList.cons Empty
                    |> Expect.equal listAfterFoldr
        ]
