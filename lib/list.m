type List['T] -> Empty | List 'T List ['T]

fn empty -> List::Empty()

fn cons x:'T xs:List['T] -> List::List(x xs)

fn singleton x:'T -> cons(x empty())

fn first xs:List['T] =
    List x _ -> x

fn rest x:List['T] =
    Empty -> x
    List _ xs -> xs

fn second x:List['T] -> first(rest(x))

fn length x:List['T] = 
    Empty -> 0
    List _ xs -> +(1 length(xs))

fn empty? x:List['T] =
    Empty -> true
    List _ _ -> false

fn concat xs:List['T] ys:List['T] =
    _, Empty -> xs
    Empty , _ -> ys
    List x xs , _ -> cons(x concat(xs ys))

fn range n:Int -> singleton(0)

fn while acc:'T update:('T)->'T condition:('T)->Bool -> acc

fn foldl f:('Acc 'T)->'Acc z:'Acc xs:List['T] -> z

fn foldr f:('T 'Acc)->'Acc z:'Acc xs:List['T] =
    _, _, Empty -> z
    _, _, List x xs -> f(x foldr(f z xs))

fn filter pred :('T)->Bool xs:List['T] =
    _, Empty -> empty()
    _, List x xs ->
        if pred(x)
            cons(x filter(pred xs))
            filter(pred xs)

type Pair['P, 'Q] -> Pair 'P 'Q

fn pair p:'P q:'Q -> Pair::Pair(p q)
fn pair_first pair:Pair['P, 'Q] = Pair x _ -> x
fn pair_second pair:Pair['P, 'Q] = Pair _ x -> x

fn list_of n:Int x:'T =
    1 , _ -> singleton(x)
    _, _ -> cons(x list_of(-(n 1) x))

fn recur_while init:'T update:('T)->'T condition:('T)->Bool ->
    if condition(init)
        recur_while(update(init) update condition)
        init

fn recursive_range n:Int = 
    0 -> empty()
    _ -> cons(n recursive_range(--(n)))

fn while_range n:Int = 
    0 -> empty()
    _ -> with k = --(n) while(
            singleton(0)
            (xs: List[Int]) -> cons(++(first(xs)), xs)
            (xs: List[Int]) -> <(first(xs), k)
        )

fn map f:('P)->'Q xs:List['P] =
    _, Empty -> empty()
    _, List x xs -> cons(f(x) map(f xs))

fn reverse xs:List['T] -> foldl(flip(cons) empty() xs)

fn reduce
        f:('Result 'T) -> 'Result
        init: 'Result
        xs: List['T]
    =
    _, _, Empty -> init
    _, _, List x xs -> f(reduce(f init xs) x)