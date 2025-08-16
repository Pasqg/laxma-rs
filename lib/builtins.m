import lib/builtins/int_builtins
import lib/builtins/float_builtins
import lib/builtins/bool_builtins

fn print x:'PrintT -> Void
fn println x:'PrintT -> Void

fn id x:'T -> x
fn . f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))
fn flip f:('C 'D)->'E -> (x:'D y:'C) -> f(y x)