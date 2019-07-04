module MyList exposing (MyList(..), cons, foldl, length, map, mapTailRec, reverse, sum, sumTailRec)

{-
   How to import in ELM REPL:
   import MyList exposing (MyList(..))
-}


type MyList a
    = Empty
    | Node a (MyList a)



-- CONS


cons : a -> MyList a -> MyList a
cons element list =
    case list of
        Empty ->
            Node element list

        Node x xs ->
            Node element list



-- LENGTH


length : MyList a -> Int
length list =
    case list of
        Empty ->
            0

        Node x xs ->
            1 + length xs



-- REVERSE
{--

reverse (Node 1 (Node 2 (Node 3 Empty)))
= reverseHelper (Node 1 (Node 2 (Node 3 Empty)))    Empty.
= reverseHelper (Node 2 (Node 3 Empty))    (Node 1 Empty).
= reverseHelper (Node 3 Empty)    (Node 2 (Node 1 Empty)).
= reverseHelper Empty    (Node 3 (Node 2 (Node 1 Empty))).
= (Node 3 (Node 2 (Node 1 Empty))).

--}


reverse : MyList a -> MyList a
reverse list =
    reverseHelper list Empty


reverseHelper : MyList a -> MyList a -> MyList a
reverseHelper original accumulator =
    case original of
        Empty ->
            accumulator

        Node x xs ->
            reverseHelper xs (Node x accumulator)



-- SUM


sum : MyList Int -> Int
sum list =
    case list of
        Empty ->
            0

        Node intValue remainingNodes ->
            intValue + sum remainingNodes


sumTailRec : MyList Int -> Int
sumTailRec list =
    sumTailRecHelper list 0


sumTailRecHelper : MyList Int -> Int -> Int
sumTailRecHelper list acc =
    case list of
        Empty ->
            acc

        Node x xs ->
            sumTailRecHelper xs (acc + x)



-- Map


map : (a -> b) -> MyList a -> MyList b
map fn list =
    case list of
        Empty ->
            Empty

        Node x xs ->
            cons (fn x) (map fn xs)


mapTailRec : (a -> b) -> MyList a -> MyList b
mapTailRec fn list =
    mapTailRecHelper fn list Empty
        |> reverse


mapTailRecHelper : (a -> b) -> MyList a -> MyList b -> MyList b
mapTailRecHelper fn list acc =
    case list of
        Empty ->
            acc

        Node x xs ->
            mapTailRecHelper fn xs (cons (fn x) acc)



-- FOLDR


foldl : (a -> b -> b) -> b -> MyList a -> b
foldl fn acc list =
    case list of
        Empty ->
            acc

        Node x xs ->
            foldl fn (fn x acc) xs
