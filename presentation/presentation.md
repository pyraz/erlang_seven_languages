# Erlang

!

Erlang Is
==========
* Functional
* Dynamic
* Concurrent
* Virtual (BEAM)
* About 20 years old

!

Erlang Promotes
===============

* Concurrency over Threads
* Immutability over Shared State
* Lightweight Processes
* "Let It Crash"

!

Dynamic
=======

Erlang does not require types to be specified, and statements end with a `.`
    
    $ erl
    1> 2 + 2.
    4
    3> "string".
    "string"
    4> [1, 2, 3].
    [1,2,3]

!

Dynamic
=======

It will do some type coercion, but nothing crazy.

    1> 2 + 2.0.
    4.0
    2> 4 + "string".
    ** exception error: bad argument in an arithmetic expression
      in operator +/2 
        called as 4 + "string"

!

Strings
=======

Strings are really Lists. If all the integers in a list can be
interpreted as characters, then it is a String. Similarly, 
matching the Head of a string yields an integer.

    1> [72, 97, 32, 72, 97, 32, 72, 97].
    "Ha Ha Ha"
    2> [Head|Tail] = "String".
    "String"
    3> Head.
    83

!

Hey, that looked like Prolog!
============================

Yes, it did. Erlang was influenced strongly by Prolog, and has similar
matching and method definitions. Like Prolog, the `=` operation is matching,
not assignment.

!

Variables
=========

Must start with an uppercase letter. Words beginning with a lowercase word
are atoms, which are like symbols in Ruby. Variables are also immutable.

    1> variable = 4.
    ** exception error: no match of right hand side value 4
    2> Variable = 4.
    4
    3> Variable.
    4

!

Variables
=========

Variables are also immutable.

    1> Variable = 1.
    2> Variable = 2.
    ** exception error: no match of right hand side value 2

!

Lists
=====

Lists use square brackets, and can contain multiple types. These are pretty
important in Erlang.

    13> [1, 2, 3].
    [1,2,3]
    14> [1, 2, "three"].
    [1,2,"three"]
    15> List = [1, 2, 3].
    [1,2,3]

!

Tuples
======

Tuples use mustache brackets, and can be used to represent key-value pairs. A
two item tuple will not match a three item tuple, and vice-versa.

    18> {one, two, three}.
    {one,two,three}
    19> {comic_strip, {name, "Calvin and Hobbes"}, 
            {character, "Spaceman Spiff"}}.

!

Pattern Matching
================

    24> Person = {person, {name, "Agent Smith"}, 
                          {profession, "Killing programs"}}.
    25> {person, {name, Name}, {profession, Profession}} = Person.
    26> Name.
    "Agent Smith"
    27> Profession.
    "Killing programs"
    
!

Pattern Matching
================
    
    28> [Head | Tail] = [1, 2, 3].
    [1,2,3]
    29> Head.
    1
    30> Tail.
    [2,3]

!

Pattern Matching
================

    32> [One, Two|Rest] = [1, 2, 3].
    [1,2,3]
    33> One.
    1
    34> Two.
    2
    35> Rest.
    [3]

!

Functions
=========

Functions require a module, and an export statment in the form of a list of
`name/number_of_parameters` definitions.

    -module(basic).
    -export([mirror/1]).
    mirror(Anything) -> Anything.

!

Functions
=========

A function can then be loaded in an `erl` console with 
the command `c(module_name).`

    4> c(basic).
    {ok,basic}
    6> basic:mirror(smiling_mug).
    smiling_mug

!

Guards
======

Erlang uses guards, which are multiple function definitions that match certain
parameters, similar to Prolog. 

    -module(matching_function).
    -export([number/1]).
    number(one) -> 1;
    number(two) -> 2;
    number(three) -> 3;
    number(_) -> 42.

!

Guards
======


An `_` is commonly used as a catch-all at the end.
A `;` is used to indicate the end of a guard, while a `.` indicates the end of
the function definitions. 
A `,` can be used to extend a function definition over
multiple lines.

!

Testing
=======

eUnit is a popular framework for unit testing Erlang. Let's use it to test drive
a simple function that recursively counts the number of words in a string.

!

Speed
=====

Erlang is fast. Let's find the factorial of `2000`.

!

Case
====

Case statemtns use pattern matching. All but the last line ends with a `;`

    case Animal of
      "elephant" -> dumbo;
      _ -> something_else
    end.


!

If
==

The if statment uses guards, with each branch being a `guard -> expression` 
clause. Again, notice the `;`

    if
      ProgramsTerminated > 0 ->
        success;
      ProgramsTerminated < 0 ->
        error
    end.

!

Anonymous Functions
===================

The fun() keyword can be used to define an anonymous function, which can then
be stored in a variable.

    16> Negate = fun(I) -> -I end.
    #Fun<erl_eval.6.13229925>
    17> Negate(1).
    -1
    18> Negate(-1).
    1


!

List Manipulation
=================

Erlang has an extesive library of functions that can be applied to lists
under the `lists` module.

  * all and any
  * dropwhile and takewhile
  * filter
  * foldl and foldr
  * foreach
  * map

And many, many more.

!

List Manipulation
================

Most of these functions are called with a function, followed by a list. For
example, to print all the items of a list:

    Numbers = [1,2,3,4].
    Print = fun(X) -> io:format("~p~n", [X]) end.
    lists:foreach(Print, Numbers).

!

List Construction
=================

Using the [Head|Tail] construct on the right side of a match (as opposed to the
left like we have seen), allows for list construction.

    double_all([]) -> [];
    double_all([First|Rest]) -> 
      [First + First|double_all(Rest)].

!

List Comprehension
=================

The list comprehension construct `||` is like a shorcut for map, since it is
such a common operation.

    Fibs = [1, 1, 2, 3, 5]
    Double = fun(X) -> X * 2 end.
    [Double(X) || X <- Fibs]

For `X` in `Fibs`, build a list that is double of X. The result is `[2,2,4,6,10]`

!

List Comprehension
=================

The list comprehension construct can be used with more than one variable as well.

    23> [{X, Y} || 
      X <- [1, 2, 3, 4], 
      X < 3, Y <- [5, 6]].
    [{1,5},{1,6},{2,5},{2,6}]

!

Concurrency
===========

This is where Erlang shines. It is easy to spawn a process and send a message to
it, and be able to monitor when it dies. When this happens, its also easy to just
spawn up a new process, thus allowing for the "Let it Crash" philosophy of
Erlang.

  * `!`
  * `receive`
  * `loop`
  * `spwan`
  * `spawn_link`
  * `register`
  * `process_flag`

!

Redundancy
==========

This distributed model allows code can be hot swapped while in production, 
monitored for failures and immediately respawned, as well as span across
multiple processes for speed.

!

You Down with OTP?
=================

The Open Telecom Platform (OTP) is a very robust library to help build Erlang
applications. Fault tolerance, scalability, transactional integrity, and
hot-swapping are all build in. It even has a full web server.

!

Wrap-Up
=======

I like Erlang.

!

Strengths
=========

  * Dynamic
  * OTP
  * Lightweight
  * Immutable
  * Let It Crash

!

Weaknesses
==========

  * Syntax
  * Popularity
  * Integration (JVM, Erjang doesn't cut it)

