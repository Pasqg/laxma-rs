#!../target/debug/laxma-rs

import core
import tests/asserts
import tests/test_bool_builtins
import tests/test_int_builtins
import tests/test_float_builtins
import tests/test_dispatch

fn test ->
    with
        bools = test_bool_builtins()
        ints = test_int_builtins()
        floats = test_float_builtins()
        dispatches = test_dispatch()
        println("All tests were successful")
