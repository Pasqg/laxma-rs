type List [ 'T ] -> Empty | List 'T List [ 'T ]

type Option [ 'T ] -> None | Some 'T

type Pair [ 'P , 'Q ] -> Pair 'P 'Q

fn empty -> List :: Empty ( )

fn cons x : 'T xs : List [ 'T ] ->
    List :: List ( x xs )

fn empty? x : List [ 'T ] =
    Empty -> true
    List _ _ -> false

fn concat xs : List [ 'T ] ys : List [ 'T ] =
    _ , Empty -> xs
    Empty , _ -> ys
    List x xs , _ -> cons ( x concat ( xs ys ) )

fn list x : 'T ->
    cons ( x empty ( ) )

fn first xs : List [ 'T ] =
    List x _ -> x

fn rest x : List [ 'T ] =
    Empty -> x
    List _ xs -> xs

fn second x : List [ 'T ] -> first ( rest ( x ) )

fn length x : List [ 'T ] = 
    Empty -> 0
    List _ xs -> + ( 1 length ( xs ) )

fn list_of n : Int x : 'T =
    1 , _ -> list ( x )
    _ , _ -> cons ( x list_of ( - ( n 1 ) x ) )

fn filter pred : ( 'T ) -> Bool xs : List [ 'T ] =
    _ , Empty -> empty ( )
    _ , List x xs ->
        if pred ( x )
            cons ( x filter ( pred xs ) )
            filter ( pred xs )

fn map f : ( 'P ) -> 'Q xs : List [ 'P ] =
    _ , Empty -> empty ( )
    _ , List x xs -> cons ( f ( x ) map ( f xs ) )

fn reduce
        f : ( 'Result 'T ) -> 'Result
        init : 'Result
        xs : List [ 'T ]
    =
    _ , _ , Empty -> init
    _ , _ , List x xs -> f ( reduce ( f init xs ) x )

fn quicksort xs : List [ Int ] =
    Empty -> empty ( )
    List pivot xs ->
        with smaller = ( x : Int ) -> < ( x pivot )
            bigger = ( x : Int ) -> >= ( x pivot )
            left = filter ( smaller xs )
            right = filter ( bigger xs )
            concat ( quicksort ( left ) concat ( list ( pivot ) quicksort ( right ) ) )

fn random seed : Int -> with x = + ( seed * ( seed 1226138780960301465 ) ) if > ( x 0 ) x * ( -- ( 0 ) x )

fn randomlist n : Int seed : Int =
    0 , _ -> empty ( )
    1 , _ -> list ( random ( seed ) )
    _ , _ -> with xs = randomlist ( -- ( n ) seed ) cons ( random ( first ( xs ) ) xs )