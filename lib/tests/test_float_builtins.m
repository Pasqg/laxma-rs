fn test_fadd ->
    with
        test1 = assert_eq(==(fadd(0.0 0.0) 0.0) "Expected fadd(0.0 0.0) == 0.0")
        test2 = assert_eq(==(fadd(3.0 0.0) 3.0) "Expected fadd(3.0 0.0) == 3.0")
        test3 = assert_eq(==(fadd(0.0 5.0) 5.0) "Expected fadd(0.0 5.0) == 5.0")
        test4 = assert_eq(==(fadd(3.0 5.0) 8.0) "Expected fadd(3.0 5.0) == 8.0")
        test5 = assert_eq(==(fadd(-3.0 0.0) -3.0) "Expected fadd(-3.0 0.0) == -3.0")
        test6 = assert_eq(==(fadd(0.0 -3.0) -3.0) "Expected fadd(0.0 -3.0) == -3.0")
        test7 = assert_eq(==(fadd(-3.0 3.0) 0.0) "Expected fadd(-3.0 3.0) == 0.0")
        test8 = assert_eq(==(fadd(3.0 -3.0) 0.0) "Expected fadd(3.0 -3.0) == 0.0")
        test9 = assert_eq(==(fadd(3.0 -5.0) -2.0) "Expected fadd(3.0 -5.0) == -2.0")
        test10 = assert_eq(==(fadd(-5.0 3.0) -2.0) "Expected fadd(-5.0 3.0) == -2.0")
        test11 = assert_eq(==(fadd(-5.0 -3.0) -8.0) "Expected fadd(-5.0 -3.0) == -8.0")
        println("test_fadd passed.")
    
fn test_fmul ->
    with
        test1 = assert_eq(==(fmul(0.0 0.0) 0.0) "Expected fmul(0.0 0.0) == 0.0")
        test2 = assert_eq(==(fmul(3.0 0.0) 0.0) "Expected fmul(3.0 0.0) == 0.0")
        test3 = assert_eq(==(fmul(0.0 5.0) 0.0) "Expected fmul(0.0 5.0) == 0.0")
        test4 = assert_eq(==(fmul(3.0 1.0) 3.0) "Expected fmul(3.0 1.0) == 3.0")
        test5 = assert_eq(==(fmul(1.0 5.0) 5.0) "Expected fmul(1.0 5.0) == 5.0")
        test6 = assert_eq(==(fmul(3.0 5.0) 15.0) "Expected fmul(3.0 5.0) == 1.05.0")
        test7 = assert_eq(==(fmul(-3.0 0.0) 0.0) "Expected fmul(-3.0 0.0) == 0.0")
        test8 = assert_eq(==(fmul(0.0 -3.0) 0.0) "Expected fmul(0.0 -3.0) == 0.0")
        test9 = assert_eq(==(fmul(-3.0 1.0) -3.0) "Expected fmul(-3.0 0.0) == -3.0")
        test10 = assert_eq(==(fmul(1.0 -3.0) -3.0) "Expected fmul(0.0 -3.0) == -3.0")
        test11 = assert_eq(==(fmul(-3.0 3.0) -9.0) "Expected fmul(-3.0 3.0) == -9.0")
        test12 = assert_eq(==(fmul(3.0 -3.0) -9.0) "Expected fmul(3.0 -3.0) == -9.0")
        test13 = assert_eq(==(fmul(-3.0 -5.0) 15.0) "Expected fmul(-5.0 -3.0) == 1.05.0")
        println("test_fmul passed.")

fn test_fsub ->
    with
        test1 = assert_eq(==(fsub(0.0 0.0) 0.0) "Expected fsub(0.0 0.0) == 0.0")
        test2 = assert_eq(==(fsub(3.0 0.0) 3.0) "Expected fsub(3.0 0.0) == 3.0")
        test3 = assert_eq(==(fsub(0.0 5.0) -5.0) "Expected fsub(0.0 5.0) == -5.0")
        test4 = assert_eq(==(fsub(3.0 5.0) -2.0) "Expected fsub(3.0 5.0) == -2.0")
        test5 = assert_eq(==(fsub(5.0 3.0) 2.0) "Expected fsub(3.0 5.0) == 2.0")
        test6 = assert_eq(==(fsub(-3.0 0.0) -3.0) "Expected fsub(-3.0 0.0) == -3.0")
        test7 = assert_eq(==(fsub(0.0 -3.0) 3.0) "Expected fsub(0.0 -3.0) == 3.0")
        test8 = assert_eq(==(fsub(-3.0 3.0) -6.0) "Expected fsub(-3.0 3.0) == 6")
        test9 = assert_eq(==(fsub(3.0 -3.0) 6.0) "Expected fsub(3.0 -3.0) == -6")
        test10 = assert_eq(==(fsub(-3.0 -3.0) 0.0) "Expected fsub(3.0 -3.0) == 0.0")
        test11 = assert_eq(==(fsub(3.0 -5.0) 8.0) "Expected fsub(3.0 -5.0) == 8.0")
        test12 = assert_eq(==(fsub(-3.0 -5.0) 2.0) "Expected fsub(3.0 -5.0) == 2.0")
        test13 = assert_eq(==(fsub(-5.0 3.0) -8.0) "Expected fsub(-5.0 3.0) == -8.0")
        test14 = assert_eq(==(fsub(-5.0 -3.0) -2.0) "Expected fsub(-5.0 -3.0) == -2.0")
        println("test_fsub passed.")

fn test_fdiv ->
    with
        test1 = assert_eq(==(fdiv(0.0 1.0) 0.0) "Expected fdiv(0.0 1.0) == 0.0")
        test2 = assert_eq(==(fdiv(0.0 3.0) 0.0) "Expected fdiv(0.0 3.0) == 0.0")
        test3 = assert_eq(==(fdiv(3.0 1.0) 3.0) "Expected fdiv(3.0 1.0) == 3.0")
        test4 = assert_eq(==(fdiv(2.0 5.0) 0.4) "Expected fdiv(2.0 5.0) == 0.4")
        test5 = assert_eq(==(fdiv(5.0 5.0) 1.0) "Expected fdiv(5.0 5.0) == 1.0")
        test6 = assert_eq(==(fdiv(-3.0 3.0) -1.0) "Expected fdiv(-3.0 3.0) == -1.0")
        test7 = assert_eq(==(fdiv(3.0 -3.0) -1.0) "Expected fdiv(3.0 -3.0) == -1.0")
        test8 = assert_eq(==(fdiv(-3.0 6.0) -0.5) "Expected fdiv(-3.0 4.0) == -0.5")
        test9 = assert_eq(==(fdiv(27.0 3.0) 9.0) "Expected fdiv(27.0 3.0) == 9.0")
        println("test_fdiv passed.")

fn test_feq ->
    with
        test1 = assert_eq(==(feq(0.0 0.0) true) "Expected feq(0.0 0.0) == true")
        test2 = assert_eq(==(feq(0.0 1.0) false) "Expected feq(0.0 1.0) == false")
        test3 = assert_eq(==(feq(-2.0 -2.0) true) "Expected feq(-2.0 -2.0) == true")
        println("test_feq passed.")

fn test_fgt ->
    with
        test1 = assert_eq(==(fgt(0.0 0.0) false) "Expected fgt(0.0 0.0) == false")
        test2 = assert_eq(==(fgt(0.0 1.0) false) "Expected fgt(0.0 1.0) == false")
        test3 = assert_eq(==(fgt(0.0 -1.0) true) "Expected fgt(0.0 -1.0) == true")
        test4 = assert_eq(==(fgt(-2.0 -3.0) true) "Expected fgt(-2.0 -3.0) == true")
        test5 = assert_eq(==(fgt(-2.0 -1.0) false) "Expected fgt(-2.0 -1.0) == false")
        test6 = assert_eq(==(fgt(-2.0 -2.0) false) "Expected fgt(-2.0 -2.0) == false")
        test7 = assert_eq(==(fgt(2.0 3.0) false) "Expected fgt(2.0 3.0) == false")
        test8 = assert_eq(==(fgt(2.0 1.0) true) "Expected fgt(2.0 1.0) == true")
        test9 = assert_eq(==(fgt(2.0 2.0) false) "Expected fgt(2.0 2.0) == false")
        println("test_fgt passed.")

fn test_fge ->
    with
        test1 = assert_eq(==(fge(0.0 0.0) true) "Expected fge(0.0 0.0) == true")
        test2 = assert_eq(==(fge(0.0 1.0) false) "Expected fge(0.0 1.0) == false")
        test3 = assert_eq(==(fge(0.0 -1.0) true) "Expected fge(0.0 -1.0) == true")
        test4 = assert_eq(==(fge(-2.0 -3.0) true) "Expected fge(-2.0 -3.0) == true")
        test5 = assert_eq(==(fge(-2.0 -1.0) false) "Expected fge(-2.0 -1.0) == false")
        test6 = assert_eq(==(fge(-2.0 -2.0) true) "Expected fge(-2.0 -2.0) == true")
        test7 = assert_eq(==(fge(2.0 3.0) false) "Expected fge(2.0 3.0) == false")
        test8 = assert_eq(==(fge(2.0 1.0) true) "Expected fge(2.0 1.0) == true")
        test9 = assert_eq(==(fge(2.0 2.0) true) "Expected fge(2.0 2.0) == true")
        println("test_fge passed.")

fn test_flt ->
    with
        test1 = assert_eq(==(flt(0.0 0.0) false) "Expected flt(0.0 0.0) == false")
        test2 = assert_eq(==(flt(1.0 0.0) false) "Expected flt(1.0 0.0) == false")
        test3 = assert_eq(==(flt(-1.0 0.0) true) "Expected flt(-1.0 0.0) == true")
        test4 = assert_eq(==(flt(-3.0 -2.0) true) "Expected flt(-3.0 -2.0) == true")
        test5 = assert_eq(==(flt(-1.0 -2.0) false) "Expected flt(-1.0 -2.0) == false")
        test6 = assert_eq(==(flt(-2.0 -2.0) false) "Expected flt(-2.0 -2.0) == false")
        test7 = assert_eq(==(flt(3.0 2.0) false) "Expected flt(3.0 2.0) == false")
        test8 = assert_eq(==(flt(1.0 2.0) true) "Expected flt(1.0 2.0) == true")
        test9 = assert_eq(==(flt(2.0 2.0) false) "Expected flt(2.0 2.0) == false")
        println("test_flt passed.")

fn test_fle ->
    with
        test1 = assert_eq(==(fle(0.0 0.0) true) "Expected fle(0.0 0.0) == true")
        test2 = assert_eq(==(fle(1.0 0.0) false) "Expected fle(1.0 0.0) == false")
        test3 = assert_eq(==(fle(-1.0 0.0) true) "Expected fle(-1.0 0.0) == true")
        test4 = assert_eq(==(fle(-3.0 -2.0) true) "Expected fle(-3.0 -2.0) == true")
        test5 = assert_eq(==(fle(-1.0 -2.0) false) "Expected fle(-1.0 -2.0) == false")
        test6 = assert_eq(==(fle(-2.0 -2.0) true) "Expected fle(-2.0 -2.0) == true")
        test7 = assert_eq(==(fle(3.0 2.0) false) "Expected fle(3.0 2.0) == false")
        test8 = assert_eq(==(fle(1.0 2.0) true) "Expected fle(1.0 2.0) == true")
        test9 = assert_eq(==(fle(2.0 2.0) true) "Expected fle(2.0 2.0) == true")
        println("test_fle passed.")

fn test_float_builtins ->
    with
        test1 = test_fadd()
        test2 = test_fmul()
        test3 = test_fsub()
        test4 = test_fdiv()

        test5 = test_feq()
        test6 = test_fgt()
        test7 = test_fge()
        test8 = test_flt()
        test9 = test_fle()

        println("-> test_float_builtins passed.")