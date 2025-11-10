import lib/builtins/int_builtins
import lib/builtins/float_builtins
import lib/builtins/bool_builtins

dispatch == x y =
    Bool, Bool -> not(xor(x y))
    Int, Int -> ieq(x y)
    Float, Float -> feq(x y)

dispatch >= x y =
    Int, Int -> ige(x y)
    Float, Float -> fge(x y)

dispatch <= x y =
    Int, Int -> ile(x y)
    Float, Float -> fle(x y)

dispatch > x y =
    Int, Int -> igt(x y)
    Float, Float -> fgt(x y)

dispatch < x y =
    Int, Int -> ilt(x y)
    Float, Float -> flt(x y)

fn print x:'PrintT -> Void
fn println x:'PrintT -> Void
fn error message:'T -> Void

fn string_length str:String -> 0
fn string_concat first:String second:String -> ""

fn id x:'T -> x
fn . f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))
fn flip f:('C 'D)->'E -> (x:'D y:'C) -> f(y x)