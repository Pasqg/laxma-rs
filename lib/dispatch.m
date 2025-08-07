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