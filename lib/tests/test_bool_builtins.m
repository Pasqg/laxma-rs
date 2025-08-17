fn test_and ->
    with
        test1 = assert_eq(==(and(true true) true) "Expected and(true true) == true")
        test2 = assert_eq(==(and(false false) false) "Expected and(false false) == false")
        test3 = assert_eq(==(and(true false) false) "Expected and(true false) == false")
        test4 = assert_eq(==(and(false true) false) "Expected and(false true) == false")
        println("test_and passed.")

fn test_or ->
    with
        test1 = assert_eq(==(or(true true) true) "Expected or(true true) == true")
        test2 = assert_eq(==(or(false false) false) "Expected or(false false) == false")
        test3 = assert_eq(==(or(true false) true) "Expected or(true false) == true")
        test4 = assert_eq(==(or(false true) true) "Expected or(false true) == true")
        println("test_or passed.")

fn test_xor ->
    with
        test1 = assert_eq(==(xor(true true) false) "Expected xor(true true) == true")
        test2 = assert_eq(==(xor(false false) false) "Expected xor(false false) == false")
        test3 = assert_eq(==(xor(true false) true) "Expected xor(true false) == false")
        test4 = assert_eq(==(xor(false true) true) "Expected xor(false true) == true")
        println("test_xor passed.")

fn test_not ->
    with
        test1 = assert_eq(==(not(true) false) "Expected not(true) == false")
        test2 = assert_eq(==(not(false) true) "Expected not(false) == true")
        println("test_not passed.")

fn test_bool_builtins ->
    with
        test1 = test_and()
        test2 = test_or()
        test3 = test_xor()
        test4 = test_not()
        println("-> test_bool_builtins passed.")