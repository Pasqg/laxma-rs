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