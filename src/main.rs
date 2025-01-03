use std::time::Instant;

use list::LinkedList;
use recur::{tail_recur, TailRecurResult};

mod list;
mod recur;

fn fibonacci_tco_helper(acc: LinkedList<i32>, n: u32) -> LinkedList<i32> {
    tail_recur((acc, n), |(acc, n)| {
        let new_acc: LinkedList<i32>;
        if acc.is_empty() {
            new_acc = acc.append(0);
        } else if acc.rest().is_empty() {
            new_acc = acc.append(1);
        } else {
            let next_num = acc.first().wrapping_add(acc.rest().first());
            new_acc = acc.append(next_num);
        }

        if n == 1 {
            return TailRecurResult::Done((new_acc, 0));
        } else {
            return TailRecurResult::Next((new_acc, n - 1));
        }
    })
    .0
}

fn fibonacci_tco(n: u32) -> LinkedList<i32> {
    return fibonacci_tco_helper(LinkedList::Empty, n);
}

fn fibonacci(n: u32) -> LinkedList<i32> {
    if n == 1 {
        return LinkedList::Empty.append(0);
    }
    if n == 2 {
        return LinkedList::Empty.append(0).append(1);
    }

    let seq = fibonacci(n - 1);
    let cur = seq.first();
    let prev = seq.rest().first();
    return seq.append(cur.wrapping_add(prev));
}

fn sum(list: &LinkedList<i32>) -> i32 {
    tail_recur((0i32, list), |(cur_sum, list)| {
        if list.is_empty() {
            return TailRecurResult::Done((cur_sum, list));
        } else {
            return TailRecurResult::Next((cur_sum.wrapping_add(list.first()), list.rest()));
        }
    })
    .0
}

fn main() {
    println!("{}", fibonacci(1));
    println!("{}", fibonacci(2));
    println!("{}", fibonacci(3));
    println!("{}", fibonacci(4));
    println!("{}", fibonacci(7));
    println!("{}", fibonacci(7).length());
    println!("{}", sum(&fibonacci_tco(7)));

    let start = Instant::now();
    let r = fibonacci_tco(1_000_000_0).length();
    let end = Instant::now();

    println!("{} in {:?}", r, (end - start));
}
