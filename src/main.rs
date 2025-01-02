use std::{fmt::Display, mem, time::Instant};

struct ListNode<T> {
    value: T,
    next: LinkedList<T>,
}

pub enum LinkedList<T> {
    NotEmpty(Box<ListNode<T>>),
    Empty,
}

impl<T> Display for LinkedList<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkedList::NotEmpty(node) => write!(f, "{},{}", node.value, node.next),
            LinkedList::Empty => write!(f, ""),
        }
    }
}

impl<T> LinkedList<T> {
    pub fn append(self, value: T) -> Self {
        LinkedList::NotEmpty(Box::new(ListNode {
            value,
            next: self,
        }))
    }

    pub fn first(&self) -> T
    where
        T: Copy,
    {
        match self {
            LinkedList::NotEmpty(node) => node.value,
            LinkedList::Empty => panic!("first() requires a non empty list"),
        }
    }

    pub fn rest(&self) -> &LinkedList<T> {
        match self {
            LinkedList::NotEmpty(node) => &node.next,
            LinkedList::Empty => &LinkedList::Empty,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            LinkedList::NotEmpty(_) => false,
            LinkedList::Empty => true,
        }
    }

    pub fn reverse(&self) -> LinkedList<T>
    where
        T: Copy,
    {
        match self {
            LinkedList::NotEmpty(_) => {
                let x = self.rest();
                let x = x.reverse();
                x.append(self.first())
            }
            LinkedList::Empty => LinkedList::Empty,
        }
    }
}

//see https://rust-unofficial.github.io/too-many-lists/first-drop.html
impl <T> Drop for ListNode<T> {
    fn drop(&mut self) {
        let mut cur_link = mem::replace(&mut self.next, LinkedList::Empty);
        while let LinkedList::NotEmpty(mut boxed_node) = cur_link {
            cur_link = mem::replace(&mut boxed_node.next, LinkedList::Empty);
        }
    }
}

enum TailRecurResult<T> {
    Next(T),
    Done(T),
}

fn tail_recur<T>(state: T, func: impl Fn(T) -> TailRecurResult<T>) -> T {
    let mut new_state = state;
    loop {
        match func(new_state) {
            TailRecurResult::Next(result) => new_state = result,
            TailRecurResult::Done(result) => {
                return result;
            }
        }
    }
}

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
            return TailRecurResult::Next((new_acc, n-1));
        }
    }).0
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
    }).0
}

fn main() {
    println!("{}", fibonacci(1));
    println!("{}", fibonacci(2));
    println!("{}", fibonacci(3));
    println!("{}", fibonacci(4));
    println!("{}", fibonacci(7));
    println!("{}", sum(&fibonacci_tco(7)));

    let start = Instant::now();
    let r = fibonacci_tco(1_000_000_0);
    let end = Instant::now();

    println!("{} in {:?}", r.first(), (end - start));
}
