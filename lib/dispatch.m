dispatch + x y =
    Float, Float -> fadd(x y)
    Int, Int -> iadd(x y)
    String, String -> string_concat(x y)

dispatch - x y =
    Float, Float -> fsub(x y)
    Int, Int -> isub(x y)

dispatch * x y =
    Float, Float -> fmul(x y)
    Int, Int -> imul(x y)

dispatch / x y =
    Float, Float -> fdiv(x y)
    Int, Int -> idiv(x y)

dispatch max x y =
    Float, Float -> if fgt(x y) x y
    Int, Int -> if igt(x y) x y

dispatch min x y =
    Float, Float -> if flt(x y) x y
    Int, Int -> if ilt(x y) x y

dispatch map x f =
    Option, _ -> option_map(x f)
    List, _ -> list_map(x f)
    Result _ -> result_map(x f)

dispatch length x =
    String -> string_length(x)
    List -> list_length(x)