fn + a:Int b:Int -> 0
fn * a:Int b:Int -> 0
fn - a:Int b:Int -> 0
fn / a:Int b:Int -> 0

fn > a:Int b:Int -> true
fn < a:Int b:Int -> true
fn >= a:Int b:Int -> true
fn <= a:Int b:Int -> true
fn == a:Int b:Int -> true

fn print x:'T -> Void
fn println x:'T -> Void

type List['T] -> Empty | List 'T List ['T]

fn cons x:'T xs:List['T] -> List::List(x xs)
fn singleton x:'T -> cons(x List::Empty())

fn range n:Int -> singleton(0)
fn while acc:'T update:('T)->'T condition:('T)->Bool -> acc


fn . f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))
fn flip f:('P 'Q)->'R -> (x:'Q y:'P) -> f(y x)