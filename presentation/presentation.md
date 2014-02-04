# Erlang

!

Erlang Is
==========
* Functional
* Dynamic
* Concurrent

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
    2> 2 + 2.0.
    4.0
    3> "string".
    "string"
    4> [1, 2, 3].
    [1,2,3]
    5> 4 + "string".
    ** exception error: bad argument in an arithmetic expression
      in operator +/2 
        called as 4 + "string"


!

Strings
=======

Strings are really Lists. If all the integers in a list can be
interpreted as characters, then its a String. Similarly, 
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

Must start with an uppercase letter. If you forget that, you'll be reminded that
the `=` is for matching, not assignment. Words beginning with a lowercase word
are atoms, which are like symbols in Ruby. Variables are also immutable.

    1> variable = 4.
    ** exception error: no match of right hand side value 4
    2> Variable = 4.
    4
    3> Variable.
    4
    4> Variable = 2.
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

Use mustache brackets, and can be used to represent key-value pairs. A
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

An `_` is commonly used as a catch-all at the end.
A `;` is used to indicate the end of a guard, while a `.` indicates the end of
the function definitions. 
A `,` can be used to extend a function definition over
multiple lines.

    -module(matching_function).
    -export([number/1]).
    number(one) -> 1;
    number(two) -> 2;
    number(three) -> 3;
    number(_) -> 42.

!

Speed
=====

Erlang is fast. Let's find the factorial of `2000`.

!

Testing
=======

eUnit is a popular framework for unit testing Erlang. Let's use it to test drive
a simple function that recursively counts the number of words in a string.

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

!

List Manipulation
=================

!

List Construction
=================

!

List Comprehension
=================

!

Concurrency
===========
