fn factorial x : Int =
    0 -> 1
    _ -> * ( x factorial ( - ( x 1 ) ) )

fn fibonacci n : Int =
    0 -> empty ( )
    1 -> list ( 0 )
    2 -> cons ( 1 list ( 0 ) )
    _ -> with
            fibonacci = with n = - ( n 1 ) fibonacci ( n )
            x = + ( first ( fibonacci ) second ( fibonacci ) )
            cons ( x fibonacci )
