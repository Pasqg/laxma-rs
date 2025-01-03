pub enum TailRecurResult<T> {
    Next(T),
    Done(T),
}

pub fn tail_recur<T>(initial_parameters: T, func: impl Fn(T) -> TailRecurResult<T>) -> T {
    let mut new_parameters = initial_parameters;
    loop {
        match func(new_parameters) {
            TailRecurResult::Next(result) => new_parameters = result,
            TailRecurResult::Done(result) => {
                return result;
            }
        }
    }
}