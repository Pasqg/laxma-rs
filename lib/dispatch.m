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

dispatch + x y =
    Float, Float -> fadd(x y)
    Int, Int -> iadd(x y)

dispatch - x y =
    Float, Float -> fsub(x y)
    Int, Int -> isub(x y)

dispatch * x y =
    Float, Float -> fmul(x y)
    Int, Int -> imul(x y)

dispatch / x y =
    Float, Float -> fdiv(x y)
    Int, Int -> idiv(x y)

dispatch map x f =
    Option, _ -> option_map(x f)
    List, _ -> list_map(x f)