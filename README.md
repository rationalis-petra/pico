# Pico
Pico is a small compiler for the Relic programming langauge. 

## Usage 
Install from release - download the latest release from the
[releases](https://github.com/rationalis-petra/pico/releases) page. Unzip the
folder and copy the executable from either the windows or linux subdirectories
into your desired install directory. 

Pico is meant to be invoked from the command line with one of three
subcommands - either `pico repl`, `pico script` or `pico eval`. Use `pico help`
to find out more about these three modes.

## Build from Source 

To build the pico from compiler from source, you will need, the
[w64devkit](https://github.com/skeeto/w64devkit) on Windows, or 
[gcc](https://www.gnu.org/software/gcc/) and
[make](https://www.gnu.org/software/make) on Linux. You also need to be using
wayland.

First, either download (or git clone) the source, then open a terminal (the
w64devkit terminal if on Windows), navigate to the source and run one of the
following commands:
- `make release` to make a release build. The executable will be `./build/release/pico`
- `make debug` to make a debug build. The executable will be `./build/debug/pico`
- `make test` to run unit tests. 


## Language Quickstart Guide

This is intended to be a very brief introduction to the syntax and semantic of the Relic language.
It is based upon the assumption that the reader is already familiar with at least one prgramming 
language, ideally from the C-family, i.e. C, C++, Java and so on. 

The Code snippets are written as expressions that one may type into the repl mode of pico.

One of the more distinctive features of the syntax is that Relic uses lisp-style function calls, 
meaning that arguments are separated with spaces (not commas) and the the function and its arguments 
are surrounded by parentheses, e.g.

```clojure
user > (print "Hello, World!")
Hello, World!
:unit
user > 
```

As you will note above, we see not only 'Hello, World!' printed, but also the return value of the
print function, which is `:unit`. This is analagous to the `void` type of other programming languages, 
as the Unit type has only one value, `:unit`, meaning that it can be used when a function wishes to
return no useful information.

Another feature of the syntax is that all operators, i.e. +, -, *, /, etc. are treated as normal
functions. Hence, `2 + 3 * 4` is written `(+ 2 (* 3 4))`. For the time being, there are separate 
operators for each of the different integral types, so 8-bit signed addidition is `i8.+`, 16-bit
unsigned addition is `u16.+`, and so on.

```clojure
user > (i64.* 10 20)
200
```

### Definitions and Functions
Some code in a programming language will be neither function or operator calls, but instead will 
be one of a various set of terms, which define things like control flow, such as `if` or `seq`, 
create values as with `struct` or `proc` or introduce new definitions, as with `def`. In Relic,
these generally have a similar structure to function calls, although there is more variation. To
introduce a new definition, for example, we write a 'function call' with `def` as the 'function',
so, to define a new value 'three', write:


```clojure
user > (def three 3)
Defined three : I64
user > three
3
```

You can see that when three was defined, relic printed the type of the value
(I64). All values in Relic have a type, which is determined ahead of time -
Relic is a statically typed language. The type 'I64' is a (signed) 64-bit
integer, while an (unsigned) 64-bit integer is U64. Relic also has other numeric
types, including U8, U16, U32, I8, I16, I32 and F32, F64 as the two
floating-point types. You will encounter more types as we explore the language
further.

Proceudres (a.k.a functions) follow a similar pattern - start a new procedure
with `proc` and follow it with an argument list, written using square brackets,
and finally follow it with the function body. Note that unlike with languages
like C, Java or Python, no `return` keyword is needed. Instead, the body of the
function is automatically returned. 

```clojure
user > (def square (proc [x] (u64.* x x)))
Defined square : Proc [I64] I64
user> (square 10)
100 
```

When writing a definition, you can emit the top-level of parenteses of the value
being deifined. The same is true for the body of a procedure, meaning the square
square function can be written more cleanly with many fewer parentheses:

```clojure
user > (def square proc [x] u64.* x x)
```

### Control Flow
Relic has several facilities available for controlling the evaluation of an expression.
The most familiar will be the `if`, which will evaluate to it's second argument
if the first argument is true, and the third argument otherwise. This can be
used, for example, to write a concise implementation of `min`: 

```clojure
user > (def min proc [x y] if (i64.< x y) x y)
Define min : Proc [I64 I64] I64
user > (min 4 3)
3
```

Another useful feature is the ability to sequence several statements. This is
done by using `seq`, followed by several expressions. A sequence will always
evaluate to the last value in that sequence. For example, the sequence below
will evaluate to '10'.

```clojure
user > (def greet proc [name] seq 
         (print "Hello, ")
         (print-ln name)
         10)
Define greet : Proc [String] Unit
user > (greet "Bob")
Hello, Bob
10
```

When making use of a sequence, it can be handy to introduce variables that are
used later on. This is done by adding `[let! var expr]` to the sequence. For
example, we may want to update our 'greet' function by having it call another 
function that gives a name to greet, as below.


```clojure
user > (def get-name proc [] "Robert")
user > (def greet proc [] seq
  [let! name (get-name)]
  (print "Hello, ")
  (print-ln name))
Define greet : Proc [] Unit
user > (greet)
Hello, Robert
```

The final control-flow construct that many languages have is an ability to loop. 
While `loop` is not (yet) part of the standard library, Relic has a more general 
construct - `labels`. Labels are an evolution of the C `goto`, and has the form:

```clojure
(labels expr [label-1 args body] [label-2 args body] ..)
```

Inside `expr` or any of the label bodies, one can `go-to` any label. If the
label takes any arguments (optional), these must also be provided to the label.
In this way, `go-to` is like a function call which never returns. Below, you can 
see labels being used 

```clojure
;; A simple loop that counts from 1 to 10
user > (def print-to proc [n]
  (labels (go-to loop 1)
    [loop [i] seq
      (print (u64.to-string i))
      (print " ")
      (if (u64.>= i n) 
          (go-to end)
          (go-to loop (u64.+ i 1)))]
    ;; end the line
    [end (print-ln "")]))
Define print-to : Proc [U64] Unti
user > (print-to 10)
1 2 3 4 5 6 7 8 9 10
:unit
```

### Types
Thus far, you have seen some simple types, such as the numeric and procedure
types. There are also types which allow combining smaller values into a larger
aggregate. The simplest way to do so is the `struct`, which associates names
(fields) with values. Below, we create a struct called 'point', which has two
integers: 'x' and 'y'.

```clojure
user > (def point struct [.x 10] [.y 4])
Define point : Struct [.x I64] [.y I64])
```

The projection '.' operator is used to get the value from a struct, e.g.

```clojure
user > (i64.+ point.x point.y)
14
```

It is also possible define a commonly used structure type, which can be useful
if we forget a field - for example, a three-dimensional point might look like:

```clojure
user > (def Point Struct [.x I64] [.y I64] [.z I64]) 
Defined Point : Type
```

Note the difference here - when defining a struct *value*, use lowercase
`struct`, while when defining a struct *type*, use the capitalized `Struct`.
This naming convention is extends to types so a value named `origin-point` may
have type `Point`. While not required, this can help you distinguish between
types and values.

Once defined, a struct type may be used to help identify errors by placing it
after the `struct` term former. In the below example, I "forget" to add an extra
field, and am provided with an appropriate error message.

```clojure
user > (def point struct Point [.x 3] [.y 5])
1 |
2 | (def point struct Point [.x 3] [.y 5])

  Structure value definition is missing the fields: z
```

Adding the missing field fixes the error.

```clojure
user > (def point struct 3DPoint [.x 3] [.y 5] [.z 10])
(struct [.x 3] [.y 5] [.z 10])
```

There is another sort of aggregate type - the `Enum`. Enum in Relic is similar
to enum in other languages, allowing us to define a type with several possible
values. A simple example is a vehicle, which may be a car, bike or truck.

```clojure
user > (def Vehicle Enum :bike :car :truck)
Defined Vehicle : Type
```

To inspect the value of an enum, use `match` - below, I use it to write a
function that calculates the number of wheels on a vehicle.

```clojure
user > (def wheels proc [vehicle] match vehicle
  [:bike 2]
  [:car 4]
  [:truck 6])
Defined wheels : Proc [(Enum :car :bike :truck)] I64
user > (wheels :car)
4
```

A feature of Relic enums which is less common is the ability to attach data to
them - for example, we may want to account for the fact that the 
[reliant robin](https://en.wikipedia.org/wiki/Reliant_Robin) has three wheels,
and so adjust the definition of the Vehicle enum:

```clojure
user > (def Vehicle Enum :bike [:car Bool] :truck)
Defined Vehicle : Type
user > (def wheels proc [vehicle] match vehicle
  [:bike 2]
  [[:car is_robin] (if is_robin 3 4)]
  [:truck 6])
```

Different branches (variants) of the enum can store different types, so we may
also decide to account for the fact that trucks can have a varying number of
tyres by storing the tyre count:

```clojure
user > (def Vehicle Enum :bike [:car Bool] [:truck U8])
Defined Vehicle : Type
user > (def wheels proc [vehicle] match vehicle
  [:bike 2]
  [[:car is_robin] (if is_robin 3 4)]
  [[:truck num_wheels] num_wheels])
```

Finally,it is worth noting that multiple pieces of data can be attached to a
variant, so we may also decide that we want to count the spare tyre that some
cars have:

```clojure
user > (def Vehicle Enum :bike [:car Bool Bool] [:truck U8])
Defined Vehicle : Type
user > (def wheels proc [vehicle] match vehicle
  [:bike 2]
  [[:car is_robin has_spare] (u8.+ (if is_robin 3 4)
                                   (if has_spare 1 0))]
  [[:truck num_wheels] num_wheels])
```

### Polymorphism and Pointers
TODO: document me!

### Traits
TODO: document me!

### Dynamic Variables
TODO: document me!

### Dynamic Unwind
TODO: document me!
