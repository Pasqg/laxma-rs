use std::{fmt::Display, mem};

use crate::recur::{tail_recur, TailRecurResult};

#[derive(Clone)]
struct ListNode<T> {
    value: T,
    next: LinkedList<T>,
}

#[derive(Clone)]
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
        LinkedList::NotEmpty(Box::new(ListNode { value, next: self }))
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

    pub fn length(&self) -> usize {
        tail_recur((0, self), |(length, list)| {
            if list.is_empty() {
                TailRecurResult::Done((length, list))
            } else {
                TailRecurResult::Next((length + 1, list.rest()))
            }
        })
        .0
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
impl<T> Drop for ListNode<T> {
    fn drop(&mut self) {
        let mut cur_link = mem::replace(&mut self.next, LinkedList::Empty);
        while let LinkedList::NotEmpty(mut boxed_node) = cur_link {
            cur_link = mem::replace(&mut boxed_node.next, LinkedList::Empty);
        }
    }
}