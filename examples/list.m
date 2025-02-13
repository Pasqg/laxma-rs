type IntList -> Empty | List Int IntList

fn is_empty x : IntList =
    Empty -> true
    List _ _ -> false

fn empty ->
    IntList :: Empty ( )

fn cons x : Int xs : IntList ->
    IntList :: List ( x xs )

fn list x : Int ->
    cons ( x empty ( ) )

fn first x : IntList =
    List fst _ -> fst

fn rest x : IntList =
    Empty -> x
    List _ xs -> xs

fn second x : IntList -> first ( rest ( x ) )

fn length x : IntList = 
    Empty -> 0
    List _ xs -> + ( 1 length ( xs ) )

fn print_list a : IntList =
    Empty -> print ( )
    List x xs -> print ( x print_list ( xs ) )