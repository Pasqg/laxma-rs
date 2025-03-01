fn id x:'T -> x
fn . f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))
fn flip f:('P 'Q)->'R -> (x:'Q y:'P) -> f(y x)

fn + a:Int b:Int -> 0
fn * a:Int b:Int -> 0
fn - a:Int b:Int -> 0
fn / a:Int b:Int -> 0

fn -- x:Int -> -(x 1)
fn ++ x: Int -> +(x 1)

fn > a:Int b:Int -> true
fn < a:Int b:Int -> true
fn >= a:Int b:Int -> true
fn <= a:Int b:Int -> true
fn == a:Int b:Int -> true

fn print x:'T -> Void
fn println x:'T -> Void

type List['T] -> Empty | List 'T List ['T]

fn empty -> List::Empty()
fn cons x:'T xs:List['T] -> List::List(x xs)
fn singleton x:'T -> cons(x empty())

fn range n:Int -> singleton(0)
fn while acc:'T update:('T)->'T condition:('T)->Bool -> acc
fn foldl f:('Acc 'T)->'Acc z:'Acc xs:List['T] -> z

fn foldr f:('T 'Acc)->'Acc z:'Acc xs:List['T] =
    _,_,Empty -> z
    _,_,List x xs -> f(x foldr(f z xs))