fn quicksort xs:List[Int] =
    Empty -> empty()
    List pivot xs ->
        with smaller = (x:Int)-> <(x pivot)
            bigger = (x:Int)-> >=(x pivot)
            left = filter(smaller xs)
            right = filter(bigger xs)
            concat(
                quicksort(left)
                concat(
                    singleton(pivot)
                    quicksort(right)
                )
            )