fn id x:'T -> x
fn . f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))
fn flip f:('C 'D)->'E -> (x:'D y:'C) -> f(y x)

fn iadd a:Int b:Int -> 0
fn imul a:Int b:Int -> 0
fn isub a:Int b:Int -> 0
fn idiv a:Int b:Int -> 0

fn fadd a:Float b:Float -> 0.0
fn fmul a:Float b:Float -> 0.0
fn fsub a:Float b:Float -> 0.0
fn fdiv a:Float b:Float -> 0.0

fn exp x:Float -> 0.0
fn log x:Float -> 0.0
fn pow x:Float e:Float -> 0.0

fn -- x:Int -> isub(x 1)
fn ++ x: Int -> iadd(x 1)

fn > a:Int b:Int -> true
fn < a:Int b:Int -> true
fn >= a:Int b:Int -> true
fn <= a:Int b:Int -> true
fn == a:Int b:Int -> true

fn print x:'T -> Void
fn println x:'T -> Void