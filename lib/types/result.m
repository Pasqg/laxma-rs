type Result['T] -> Error String | Ok 'T

fn is_ok result:Result['T] =
    Error _ -> false
    Ok _ -> true

fn is_err result:Result['T] =
    Error _ -> true
    Ok _ -> false

fn as_ok result:Result['T] =
    Ok ok -> ok

fn as_err result:Result['T] =
    Error error -> error

fn result_map result:Result['P] f:('P)->'Q =
    Error error, _ -> Result::Error(error)
    Ok x, _ -> Result::Ok(f(x))