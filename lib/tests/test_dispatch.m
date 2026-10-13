fn test_max_float ->
    with
        test1 = assert_eq(==(max(0.0 0.0) 0.0) "Expected max(0.0 0.0) == 0.0")
        test2 = assert_eq(==(max(3.0 0.0) 3.0) "Expected max(3.0 0.0) == 3.0")
        test3 = assert_eq(==(max(0.0 5.0) 5.0) "Expected max(0.0 5.0) == 5.0")
        test4 = assert_eq(==(max(3.0 5.0) 5.0) "Expected max(3.0 5.0) == 5.0")
        test5 = assert_eq(==(max(-3.0 0.0) 0.0) "Expected max(-3.0 0.0) == 0.0")
        test6 = assert_eq(==(max(0.0 -3.0) 0.0) "Expected max(0.0 -3.0) == 0.0")
        test7 = assert_eq(==(max(-3.0 3.0) 3.0) "Expected max(-3.0 3.0) == 3.0")
        test8 = assert_eq(==(max(3.0 -3.0) 3.0) "Expected max(3.0 -3.0) == 3.0")
        test9 = assert_eq(==(max(3.0 -5.0) 3.0) "Expected max(3.0 -5.0) == 3.0")
        test10 = assert_eq(==(max(-5.0 3.0) 3.0) "Expected max(-5.0 3.0) == 3.0")
        test11 = assert_eq(==(max(-5.0 -3.0) -3.0) "Expected max(-5.0 -3.0) == -3.0")
        println("test_max_float passed.")

fn test_min_float ->
    with
        test1 = assert_eq(==(min(0.0 0.0) 0.0) "Expected min(0.0 0.0) == 0.0")
        test2 = assert_eq(==(min(3.0 0.0) 0.0) "Expected min(3.0 0.0) == 0.0")
        test3 = assert_eq(==(min(0.0 5.0) 0.0) "Expected min(0.0 5.0) == 0.0")
        test4 = assert_eq(==(min(3.0 5.0) 3.0) "Expected min(3.0 5.0) == 3.0")
        test5 = assert_eq(==(min(-3.0 0.0) -3.0) "Expected min(-3.0 0.0) == -3.0")
        test6 = assert_eq(==(min(0.0 -3.0) -3.0) "Expected min(0.0 -3.0) == -3.0")
        test7 = assert_eq(==(min(-3.0 3.0) -3.0) "Expected min(-3.0 3.0) == -3.0")
        test8 = assert_eq(==(min(3.0 -3.0) -3.0) "Expected min(3.0 -3.0) == -3.0")
        test9 = assert_eq(==(min(3.0 -5.0) -5.0) "Expected min(3.0 -5.0) == -5.0")
        test10 = assert_eq(==(min(-5.0 3.0) -5.0) "Expected min(-5.0 3.0) == -5.0")
        test11 = assert_eq(==(min(-5.0 -3.0) -5.0) "Expected min(-5.0 -3.0) == -5.0")
        println("test_min_float passed.")

fn test_max_int ->
    with
        test1 = assert_eq(==(max(0 0) 0) "Expected max(0 0) == 0")
        test2 = assert_eq(==(max(3 0) 3) "Expected max(3 0) == 3")
        test3 = assert_eq(==(max(0 5) 5) "Expected max(0 5) == 5")
        test4 = assert_eq(==(max(3 5) 5) "Expected max(3 5) == 5")
        test5 = assert_eq(==(max(-3 0) 0) "Expected max(-3 0) == 0")
        test6 = assert_eq(==(max(0 -3) 0) "Expected max(0 -3) == 0")
        test7 = assert_eq(==(max(-3 3) 3) "Expected max(-3 3) == 3")
        test8 = assert_eq(==(max(3 -3) 3) "Expected max(3 -3) == 3")
        test9 = assert_eq(==(max(3 -5) 3) "Expected max(3 -5) == 3")
        test10 = assert_eq(==(max(-5 3) 3) "Expected max(-5 3) == 3")
        test11 = assert_eq(==(max(-5 -3) -3) "Expected max(-5 -3) == -3")
        println("test_max_int passed.")

fn test_min_int ->
    with
        test1 = assert_eq(==(min(0 0) 0) "Expected min(0 0) == 0")
        test2 = assert_eq(==(min(3 0) 0) "Expected min(3 0) == 0")
        test3 = assert_eq(==(min(0 5) 0) "Expected min(0 5) == 0")
        test4 = assert_eq(==(min(3 5) 3) "Expected min(3 5) == 3")
        test5 = assert_eq(==(min(-3 0) -3) "Expected min(-3 0) == -3")
        test6 = assert_eq(==(min(0 -3) -3) "Expected min(0 -3) == -3")
        test7 = assert_eq(==(min(-3 3) -3) "Expected min(-3 3) == -3")
        test8 = assert_eq(==(min(3 -3) -3) "Expected min(3 -3) == -3")
        test9 = assert_eq(==(min(3 -5) -5) "Expected min(3 -5) == -5")
        test10 = assert_eq(==(min(-5 3) -5) "Expected min(-5 3) == -5")
        test11 = assert_eq(==(min(-5 -3) -5) "Expected min(-5 -3) == -5")
        println("test_min_int passed.")

fn test_dispatch ->
    with
        test1 = test_max_float()
        test2 = test_min_float()
        #test3 = test_max_int()
        #test4 = test_min_int()

        println("-> test_dispatch passed.")