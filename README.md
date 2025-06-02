# Laxma-rs

A simple functional programming language with rust backend.

## Basic syntax

The grammar is designed to be concise and non-ambiguous. It has to be easy to read and quick to write. For this reason:
- Indentation is not syntactically meaningful
- Only whitespaces should separate tokens as much possible (minimum amount of parentheses, no end of statement tokens)

## Types

The base of Laxma is a strong type system which is designed to catch errors and bugs at compile time.

In Laxma, all types are sets of unique items, implemented as tagged unions. The type system is heavily inspired by Rust and Haskell.

A type can be created using type definitions:

```python
type Boolean -> True | False
```

This defines a type Boolean whose instances can be literals True or False. Very basic types can be defined in this way, including number sets although these cannot be defined by the user.

All variants of a type (items of the set) are tagged with an identifier. This is a requirements also for types with only one variant, for example for records. By convention, type and variant names are capitalised.

A record can be created as follows:

```rust
type Person -> Person String Int
```

This creates Person type with two fields for name and age, respectively of type String and Int. Note that the variant name 'Person' doesn't need to be the same as the type name.

We can also create recursive types. For example, let's add a father to our person. This will be a field with type Person:

```rust
type Person -> Person String Int Person
```

Note that while this is a valid type definition, it recurses infinitely and thus cannot be instantiated given we cannot construct a Person without constructing it's father, and the father of the father etc. Nevertheless it illustrates the syntax.

Let's build instead a list of integers:

```rust
type IntList -> Empty | List Int IntList
```

This describes a type IntList that can either be Empty or a record that contains an Int and another IntList. This is effectively a linked list, created as a union of a literal Empty and a record List. Recursion base case is the Empty variant, which doesn't require a list to be instantiated.

More complex types can be built using type parameters. A generic list can be defined as:

```rust
type List['T] -> Empty | List 'T List['T]
```

This is very similar to the previous IntList definition, with the added ['T] which identifies a type parameter.

### Type constructors

Variants of a certain type can be constructed using the type constructor expression. The syntax is `<Type name>::<Variant name>(<values for fields>)`.

```rust
List::Empty()
List::List(1, List::Empty())
List::List(2, List::List(1, List::Empty()))
```

Type constructors also come in form of functions with naming convention `<Type name>.<Variant name>`. Note the `.` is not a special operator but just a character in the function name. As all other functions, the type constructor functions can be used as function arguments, while type constructor expressions cannot.

```rust
List.Empty()
List.List(1, List.Empty())
List.List(2, List.List(1, List.Empty()))
```

### Built-in types

- Bool: equivalent to bool in rust
- Int: equivalent to i64 in rust
- Float: equivalent to f64 in rust
- String: equivalent to String in rust
- Void: equivalent to () in rust

## Functions

The most basic function has a name, a list of arguments (name + type) and an expression for the body:

```rust
fn increment x:Int -> +(1 x)
```

An expression is a literal (number or string) or a function call (whose parameters are also expressions, separated by whitespace or comma). Type constructors are also functions.

A function is also a variant of the special type class Function. In this case increment is a variant of type `Function[Int] -> Int`.

Functions can also have multiple arguments, separated by whitespace:

```rust
fn add x:Int y:Int -> +(x y)
```

### Pattern matching

Functions can have multiple bodies by using pattern matching:

```rust
fn empty? x:List['T] =
    Empty -> true
    List _ _ -> false

fn length x:List['T] =
    Empty -> true
    List _ xs -> +(1 length(xs))

fn first x:List['T] =
    List x _ -> x

fn rest x:List['T] =
    List _ xs -> xs
```

Note, all bodies must return the same type for the function to be valid, but the pattern matching doesn't need to be exhaustive. Missing patterns will either be flagged by the compiler if possible for example when explicitly calling `first(List::Empty())`, or will result in a runtime error when the argument value cannot be known at compile time.

Pattern matching must be done on all function arguments, with each destructuring separated by comma. Multiple cases are also supported, delimited with '|'

```rust
type Byte -> b0 | b1

fn xor a:Byte b:Byte =
    b0, b0 | b1, b1 -> b0
    b0, b1 | b1, b0 -> b1
```

Identifiers in a pattern must match specific variants of the argument type:

```rust
Empty -> ... // Matches only variant Empty
List _ _ -> ... // Matches only variant List (and must have as many bindings as values in the variant)
SomethingElse -> ... // Gives error because no variant SomethingElse
```

This extends to bools and numbers (with the exclusion of destructuring for obvious reasons):

```rust
1 -> ... // Good
n -> ... // Error, it is redundant re-binding of an argument
```

**Important note:** pattern matching destructuring identifiers shadow top-level ones:

```rust
fn first x:List['T] =
    List x _ -> x // in this arm, x refers to the first element
```

### With expression

The with block is an expression that allows additional bindings for a certain expression. This can be used anywhere an expression is accepted: as function body and as functional call parameter.

The with block syntax requires a list of 'identifier = expression' bindings and an expression to return:

```
fn fibonacci n:Int =
    0 -> empty()
    1 -> list(0)
    2 -> cons(1 list(0))
    _ -> with
            xs = with n = -(n 1) fibonacci(n)
            x = +(first(xs) second(xs))
            cons(x xs)
```

Although nested with expressions are allowed, it is better to avoid them for readibility.

### Cast expression

Safe cast expression that always resolves to a concrete type or a type variable
```
cast 3 as Int -> Int
cast 4 as Float -> error!
cast x:'T as 'B -> 
cast empty() as List['B] -> List['B]
```

### Return type inference

In Laxma, the return type of a function is not explicitly stated.

For non-recursive functions, type inferrence is straighforward: the return type of a function is equal to the type of the expression.

For tail recursive and mutually recursive functions, the return type is inferred from the first available base case.

Consider this example of a basic recursive function:

```rust
fn factorial x:Int =
    0 -> 1
    n -> *(n factorial(-(n 1)))
```

The compiler will initialise its function type table with built-ins and with one entry for factorial with unknown type. As it examines the first pattern matching branch, it will set factorial type to Int (inferred from constant 1). When it checks the second branch, it will also infer Int (from function *).

Let's consider now a tail-recursive list length function:

```rust
fn length list:List['T] -> length_tail(list 0)

fn length_tail list:List['T] x:Int =
    NonEmpty _ rest, _ -> length_tail(rest +(x 1))
    Empty, _ -> x
```

Here when we infer the type of length, we find that length_tail has type unknown. We then try to infer the type of length_tail. Examining the first branch, we cannot infer the return type (i.e. it is unknown) but the second branch returns an Int. Hence the compiler will infer Int for the whole function.

```rust
fn factorial x:Int =
    0 -> 1
    n -> *(n factorial(-(n 1)))
```

### Function operations

Functions can be passed as function parameters or returned by another function.

```rust
fn compose f:('Q)->'R g:('P)->'Q x:'P -> f(g(x))
```

When resolving function calls, function arguments have precedence over user defined functions and built-ins. In other words, function arguments shadow the caller scope.

#### Lambdas

An anonymous function can be created as such:

```rust
fn compose f:('Q)->'R g:('P)->'Q -> (x:'P) -> f(g(x))
```

Lambdas capture the outside environment in the same way as inner expression get outer bindings

```rust
fn quick_sort x : IntList =
    Empty -> empty ( )
    List pivot xs ->
        // pivot is captured in the lambdas
        with smaller = ( x : Int ) -> < ( x pivot )
            bigger = ( x : Int ) -> >= ( x pivot )
            left = filter ( smaller xs )
            right = filter ( bigger xs )
            concat ( quick_sort ( left ) concat ( list ( pivot ) quick_sort ( right ) ) )
```

## REPL

Running the REPL with cargo is straightforward:
```
cargo run
```

### Commands

| Command | Description |
| --- | --- |
| /load | Loads one or multiple files or directories into the REPL |
| /quit, /exit | Closes the REPL |

### Loading builtins

Most primitives are available without loading any file, but it is recommended to load:

```
/load lib/core
```

These will eventually be loaded automatically at REPL start-up.