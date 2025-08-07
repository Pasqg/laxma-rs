type Option['T] -> None | Some 'T

fn is_none o:Option['T] =
    None -> true
    Some _ -> false

fn is_some o:Option['T] =
    None -> false
    Some _ -> true

fn option_map o:Option['P] f:('P)->'Q =
    None, _ -> Option::None()
    Some x, _ -> Option::Some(f(x))
    