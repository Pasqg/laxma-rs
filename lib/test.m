#!../target/debug/laxma-rs

import lib/core
import lib/tests/asserts
import lib/tests/test_bool_builtins
import lib/tests/test_int_builtins
import lib/tests/test_float_builtins

fn test ->
    with
        bools = test_bool_builtins()
        ints = test_int_builtins()
        floats = test_float_builtins()
        println("All tests were successful")
