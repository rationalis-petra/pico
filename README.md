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

This is intended to be a very brief introduction to the syntax and semantics of
the Relic language. It is based upon the assumption that the reader is already
familiar with at least one prgramming language, ideally from the C-family, i.e.
C, C++, Java and so on.

The Code snippets are written as expressions that one may type into the repl
mode of pico. Recall above, it can be seen that this can be run with `pico repl`. 

When reading a Relic program, the most immediately apparent difference with
other languages is the syntax, or appearance of the language. To translate from
a 'normal' language to Relic, we follow a thre-step process - consider the
following "python-style" code:

```python
print-ln(to-string(2 + 3))
```

### Step 1: Convert Operators

In Relic, arithemetic operators such as '+', '-' and '*' are actually just
regular functions. They therefore use the same syntax as a reular function call

```clojure
print-ln(to-string(+ (2, 3))
```

### Step 2: Move Parentheses

Another difference between Relic and other languages is that the function is included
in the parentheses of a function-call, so the expression now becomes

```clojure
(print-ln, (to-string, (+, 2, 3))
```


### Step 3: Remove Commas

The final difference is that Relic does not use commas to separate function
arguments, only whitespace, so we can safely remove them:

```clojure
(print-ln (to-string (+ 2 3))
```

Indeed, if you run `pico repl` and type the above into the input, you should see
'5' printed to the console.

```clojure
user > (print-ln (to-string (+ 2 3))
5
:unit
```

The reason that the text ':unit' appears below '5' will become apparent in later sections.

### Definitions and Functions
A program is, of course, not enturely made of function calls, as we also will
need conditionals like `if`, ways of looping such as `for` and `while`, and ways
of *defining* new values. Definitions are the focus of this section. To
introduce a new definition, write `(def <name> <value>)` 

```clojure
user > (def pi 3.14159)
Defined pi : F64
user > pi
3.14159
```

You can see that when pi was defined, relic printed the type of the value, which
was F64, meaning a 64-bit floating point number. Relic also has other numeric
types, including unsigned integers (U8, U16, U32, U64), signed integers (I8,
I16, I32, I64) and floating point numbers F32, F64. You will encounter more
types as we explore the language further.

Proceudres (a.k.a functions) follow a similar pattern to the definitions - to start a new procedure,
use `proc` and follow it with an argument list, written using square brackets,
and finally follow it with the function body. Note that unlike with languages
like C, Java or Python, no `return` keyword is needed. Instead, the body of the
function is automatically returned. 

```clojure
user > (def square (proc [x] (f64.* x x)))
Defined square : Proc [I64] I64
user> (square pi)
9.869588 
```

When writing a definition, you can omit the parenteses of the value being
deifined. The same is true for the body of a procedure, meaning the square 
square function can be written more cleanly with many fewer parentheses:

```clojure
user > (def square proc [x] f64.* x x)
```

### Control Flow
Relic has several facilities available for controlling the evaluation of an expression.
The most familiar will be the `if`, although it works slightly differently to
the usual `if/else` of c-style programming languages. Instead, the `if` is
structure as `(if <condition> <run-if-true> <run-if-false>)`. This can be
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
Defined greet : Proc [String] Unit
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
Defined get-name : Proc [] String
user > (def greet proc [] seq
  [let! name (get-name)]
  (print "Hello, ")
  (print-ln name))
Define greet : Proc [] Unit
user > (greet)
Hello, Robert
:unit
```

The final control-flow construct that many languages have is an ability to loop. 
Usually, there is a `for` loop and a `while` loop. In Relic, these are joined
into a single looping construct, called `loop`. A simple for-loop is below:

```clojure
user > (loop [for i from 1 upto 10]
  (print-ln (to-string i)))
```

We can also add a `while` to our loop, if we so desire, which will cause an
early termination.

```clojure
user > (loop [for i from 1 upto 10]
             [while (< i 8)]
  (print-ln (to-string i)))
```

We can even add multiple loops in parallel. As with `while`, the loop will 
terminate if any of the individual loops (termed 'drivers') terminate.

```clojure
user > (loop [for i from 1 upto 10]
             [for j from 10 downto 1]
  (seq 
    (print "i, j: ")
    (print (to-string i))
    (print ", ")
    (print-ln (to-string j))))
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
user > (+ point.x point.y)
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
user > (def point struct Point [.x 3] [.y 5] [.z 10])
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
  [[:car is_robin has_spare] (+ (if is_robin 3 4)
                                (if has_spare 1 0))]
  [[:truck num_wheels] num_wheels])
```

### Polymorphism and Pointers
Often, we encounter times when it is desirable to have very similar code for
different types. A prime example is the `list` - we may want to have a list of
integers, another of floats, and a list of strings. Below, you can see some code
which does exactly that. 

```clojure
;; Open is similar to 'import' in other languages, by "opening" the list module,
;; we gain access to the functions inside
user > (open data.list)
user > (def str-list (list "bannana" "apple" "orange"))
Defined str-list : List String
user > (def ilist (data.list -10 5 6))
Defined ilist : List I64 
user > (def flist (list -10.5 5000.5 3.14))
Defined list : List F64 
```

We can then use the same funcion - `elt` to access an element of the list

```clojure
user > (print-ln (elt 0 slist))
"bannana"
:unit
user > (elt 1 ilist)
4
user > (elt 2 flist)
3.14
```

Both `list` and `elt` can work across multiple types like this because they make
use of *polymorphism*. Polymorphism in Relic is enabled by two features:
*polymorphic functions* and *type families*. 

#### Polymorphic Functions
The simplest polymorphic function is the identity function `id`, which would be
used as follows:

```clojure
user > (id 3)
3
user > (id "test")
"test"
```

The `id` function is written below, and can be read as "for any type A, id takes
in a value 'x' of type A, and returns x'. 
 
```clojure
user > (def id all [A] proc [(x A)] x)
Defined id: All [A] (Proc [A] A)
```

The `id` can now be applied to values as you saw above - we can even apply id to itself!

```clojure
user > (id 3.14159)
3.14159
user > (id id)
#<all 0x7ba21e80a000>
```

Two foundational polymorphic functions in Relic are `load` and `store`. These
form the basis of Relic's ability to interact with the `Address` type. Let's
first grab an address by allocating 8 bytes of memory:

```clojure
user > (def addr malloc 8)
Defined addr : Address
```

Now, it is possible to store a 64 bit integer with the `store` funtion. 

```clojure
user > (store addr 345)
Defined addr : Address
```

Load, meanwhile, has the type `(All [A] Proc [Addr] A)`. If we try and use it
directly, we will receive an "ambiguous type" error. 

```clojure
user > (load addr)
Ambiguous type: the type of (load addr) could not be deduced.
```

To explicitly provide the type, we use curly braces, `{}`. Like square brackets,
which denote special information that is not executed directly, curly braces
denote that implicit (optional) information is being explicitly provided - in
this case, the types.

```clojure
user > (load {I64} addr)
345
```

Note that the address type does not care about the underlying value, we can load
it as a "different" type. For example, the 345 that we stored at addr has binary
representation `101011001`. Taking only the lower 8 bits, we get `1011001`,
or 89. We can verify by loading `addr` with the `U8` type:

```clojure
user > (load {U8} addr)
89
```

#### Type Families
Polymorphic functions, as seen above, give us the ability to write functions
that can work across multiple data-types. But, they do not allow us to write
containers, such as lists or sets, that work with multiple data-types. This is
the role of Type Families. Type families are introduced with the `Family`
keyword, which functions similarly to the `All` keyword. The example below uses
type families to create a basic `Pair` type, representing a pair of any two values: 

```clojure
user > (def Pair Family [A B] Struct [.first A] [.second B])
defined Pair : Kind [Type Type] Type
```

Pairs can be constructed using the normal `struct` syntax, but type families
more generally are most useful when paired with polymorphic functions:

```clojure
user > (def my-pair struct (Pair I64 F32) [.x -12] [.y 3.1214])
defined my-pair : Struct [.first I64] [.second F32]
user > (def pair all [A B] proc [(x A) (y b)] 
         struct (Pair A B) [.first I64] [.second F32])
Defined pair : All [A B] (Proc [A B] (Struct [.first A] [.second B]))
user > (pair 4.56 13)
(struct [.first 4.56] [.second 13])
```


### Named, Distinct and Opaque Types 
TODO: document me!

### Traits
TODO: document me!

### Dynamic Variables
TODO: document me!

### Dynamic Unwind
TODO: document me!

### Small and Large Programs: Scripts and Modules
TODO: document me!

### Standard Library
TODO: document me!
