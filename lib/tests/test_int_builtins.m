fn test_iadd ->
    with
        test1 = assert_eq(==(iadd(0 0) 0) "Expected iadd(0 0) == 0")
        test2 = assert_eq(==(iadd(3 0) 3) "Expected iadd(3 0) == 3")
        test3 = assert_eq(==(iadd(0 5) 5) "Expected iadd(0 5) == 5")
        test4 = assert_eq(==(iadd(3 5) 8) "Expected iadd(3 5) == 8")
        test5 = assert_eq(==(iadd(-3 0) -3) "Expected iadd(-3 0) == -3")
        test6 = assert_eq(==(iadd(0 -3) -3) "Expected iadd(0 -3) == -3")
        test7 = assert_eq(==(iadd(-3 3) 0) "Expected iadd(-3 3) == 0")
        test8 = assert_eq(==(iadd(3 -3) 0) "Expected iadd(3 -3) == 0")
        test9 = assert_eq(==(iadd(3 -5) -2) "Expected iadd(3 -5) == -2")
        test10 = assert_eq(==(iadd(-5 3) -2) "Expected iadd(-5 3) == -2")
        test11 = assert_eq(==(iadd(-5 -3) -8) "Expected iadd(-5 -3) == -8")
        println("test_iadd passed.")
    
fn test_imul ->
    with
        test1 = assert_eq(==(imul(0 0) 0) "Expected imul(0 0) == 0")
        test2 = assert_eq(==(imul(3 0) 0) "Expected imul(3 0) == 0")
        test3 = assert_eq(==(imul(0 5) 0) "Expected imul(0 5) == 0")
        test4 = assert_eq(==(imul(3 1) 3) "Expected imul(3 1) == 3")
        test5 = assert_eq(==(imul(1 5) 5) "Expected imul(1 5) == 5")
        test6 = assert_eq(==(imul(3 5) 15) "Expected imul(3 5) == 15")
        test7 = assert_eq(==(imul(-3 0) 0) "Expected imul(-3 0) == 0")
        test8 = assert_eq(==(imul(0 -3) 0) "Expected imul(0 -3) == 0")
        test9 = assert_eq(==(imul(-3 1) -3) "Expected imul(-3 0) == -3")
        test10 = assert_eq(==(imul(1 -3) -3) "Expected imul(0 -3) == -3")
        test11 = assert_eq(==(imul(-3 3) -9) "Expected imul(-3 3) == -9")
        test12 = assert_eq(==(imul(3 -3) -9) "Expected imul(3 -3) == -9")
        test13 = assert_eq(==(imul(-3 -5) 15) "Expected imul(-5 -3) == 15")
        println("test_imul passed.")

fn test_isub ->
    with
        test1 = assert_eq(==(isub(0 0) 0) "Expected isub(0 0) == 0")
        test2 = assert_eq(==(isub(3 0) 3) "Expected isub(3 0) == 3")
        test3 = assert_eq(==(isub(0 5) -5) "Expected isub(0 5) == -5")
        test4 = assert_eq(==(isub(3 5) -2) "Expected isub(3 5) == -2")
        test5 = assert_eq(==(isub(5 3) 2) "Expected isub(3 5) == 2")
        test6 = assert_eq(==(isub(-3 0) -3) "Expected isub(-3 0) == -3")
        test7 = assert_eq(==(isub(0 -3) 3) "Expected isub(0 -3) == 3")
        test8 = assert_eq(==(isub(-3 3) -6) "Expected isub(-3 3) == 6")
        test9 = assert_eq(==(isub(3 -3) 6) "Expected isub(3 -3) == -6")
        test10 = assert_eq(==(isub(-3 -3) 0) "Expected isub(3 -3) == 0")
        test11 = assert_eq(==(isub(3 -5) 8) "Expected isub(3 -5) == 8")
        test12 = assert_eq(==(isub(-3 -5) 2) "Expected isub(3 -5) == 2")
        test13 = assert_eq(==(isub(-5 3) -8) "Expected isub(-5 3) == -8")
        test14 = assert_eq(==(isub(-5 -3) -2) "Expected isub(-5 -3) == -2")
        println("test_isub passed.")

fn test_idiv ->
    with
        test1 = assert_eq(==(idiv(0 1) 0) "Expected idiv(0 1) == 0")
        test2 = assert_eq(==(idiv(0 3) 0) "Expected idiv(0 3) == 0")
        test3 = assert_eq(==(idiv(3 1) 3) "Expected idiv(3 1) == 3")
        test4 = assert_eq(==(idiv(2 5) 0) "Expected idiv(2 5) == 0")
        test5 = assert_eq(==(idiv(5 5) 1) "Expected idiv(5 5) == 1")
        test6 = assert_eq(==(idiv(-3 3) -1) "Expected idiv(-3 3) == -1")
        test7 = assert_eq(==(idiv(3 -3) -1) "Expected idiv(3 -3) == -1")
        test8 = assert_eq(==(idiv(-3 4) 0) "Expected idiv(-3 4) == 0")
        test9 = assert_eq(==(idiv(27 3) 9) "Expected idiv(27 3) == 9")
        println("test_idiv passed.")

fn test_int_builtins ->
    with
        test1 = test_iadd()
        test2 = test_imul()
        test3 = test_isub()
        test4 = test_idiv()
        println("-> test_int_builtins passed.")