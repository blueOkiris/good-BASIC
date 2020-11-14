# Grammar Definition

## What needs to be done

Okay so now we have a basic structure for things.

We know the data types and declarations
We know there will be function calls
We know there will be some kind of loop and some kind of if statement
We know there's some built in operators and functions

Now we need to define how all of that stuff actually works. So first let's define all of the specifics of those things

## Data Type Grammar

Okay so first data types:
1. Integers
2. Floating Point Numbers
3. Strings
4. Arrays
5. Function Types
6. Record Types
7. Composition Types

Declarations work like `let` + optional `mut` + type name + identifier + optional `=` + expression. For mutables, they can be reassigned with identifier + `=` + expression.

Now certain types are not declared in the same way. Functions can be declared using lambda and record types and composition types can be declared using a special notation.

Lambdas are as follows:
```
let fn(<type1>, ...) <ret-type> <ident> = lambda (<arg1>, ...) <ret-type>
    ...
end
```

Record types and composition types are as follows:

```
let <custom-type-name> <ident> = data(<member1>, ...)
```

If it's a composition type, not a record type, then an instance can be defined by assigning it to a module name: `let <comp-type> <ident> = <mod-name>`. It will throw an error of course if the module doesn't implement that interface.

Of course, how do you get these new types? Well you have to *define* them

```
def rec <type-name>
    <type-name> <ident>
end

def comp <interface-name>
    <ident> ( <arg type>, ... ) <ret-type>
    ...
end
```

You can also define functions instead of using lambda syntax:

```
def fn <ident> ( <arg type>, ... ) <ret-type>
    <statement>
    ...
end
```

The other data types are pretty much what you'd expect. Integers are just digits, Floats are digits with a decimal somewhere, and strings are anything in-between single quotes with escape characters.

Arrays will be declared and altered with functions from a native library that gets imported. They're "special"

The other thing I want to point out is what exactly the data is. To make it easier to store functions, I'm taking a page out of python's book and making everything a pointer to data. So when you say `let mut int foo = 10; foo++`, you're essentially doing `int *foo; *foo = 10; (*foo)++;`

That way

```
let fn(int, int) int sum = lambda(int x, int y) int
    return x + y
end
```

Could turn into

```
int lambda_1(int x, int y) {
    return x + y
}
...
int (*)(int, int) sum = lambda_1;
```

Or something like that. This means everything is essentially passed by reference by passing its pointer by value, so how would one actually pass by value? Well, you don't. You should have data immutable, so passing by value wouldn't really solve anything. It's as if everything is C++'s `const &<type-name> <ident>`. It's safe to assume if you're taking a mutable as a parameter, then you intend to modify it, otherwise there really isn't a reason to anyway.

The exception is return. Return will always get the value of the data. When using let to get the value of a function call, a new pointer will be created with the data copied from the old pointer.

## Statements and Control Flow

Next. Function calls. `call` + function name + arguments. In order to wrap these within other expressions, naturally it can all be wrapped in parentheses: `( call <func-name> <arg1> ... )`

I'm going to limit loops to while and for loops. There were lots of kinds of loops, but in modern programming, you really only see iterative loops and conditional loops, so to keep everything simple, I'll just have those two.

For loops work like they traditionally have in basic: `FOR` + *scoped* mutable declaration and assignment + `TO` + ending value for variable + optional `STEP` and change amount followed by instructions and `END`. __However__, I'm replacing `TO` and a value with `WHILE` and a condition and requiring `STEP`. This allows for loops to be used with all variable types, instead of just integer types, for instance: `for a='*' while a != '*****' step a += '*'`

While loops are simpler:

```
while <condition>
    ...
end
```

If statements work like while, but with optional else:

```
if <condition>
    ...
else
    ...
end
```

And of course all of these can be nested

Builtin stuff. What will I have?
- some basic math operators (which have uses outside integers like concatenation): +, -, *, /, %, ++, --
- Boolean operators: ==, !=, >, <, >=, <=, &&, ||, !
- Bitwise operators: &, |, ^, ~, <<, >>
- I/O: print, input, write, read where write and read refer to files

## Modules

So of course, there's the module system and the starting lines

First comes optional `IMPORTS` where you can import another module

Next you have `EXPORTS` plus a comment delimited list of functions, compositions, or record types and an optional `IMPLEMENTS` plus a comment delimited list of interfaces.

They must come in that order

## EBNF

Now to actually write up an ebnf grammar to follow while writing the parser

```
<module>        ::= { <import> } /\n+/
                    <export> [ <implement> ] /\n+/
                    { <definition> /\n+/ }
<import>        ::= 'import' <ident>
<export>        ::= 'exports' <ident-list>
<implement>     ::= 'implements' <ident-list>
<ident-list>    ::= <ident> { ',' <ident> }

<definition>    ::= <func-def> | <comp-def> | <rec-def>
<func-def>      ::= 'def' 'fn' <ident> '(' [ <type-arg-list> ] ')' <type-name>
                    /\n+/
                        { <statement> /\n+/ } /\n+/
                    '<end>'
<type-arg-list> ::= <type-name> <ident> { ',' <type-name> <ident> }
<type-name>     ::= 'int' | 'float' | 'str'
                  | 'fn' '(' [ <type-list> ] ')' <type-name>
                  | '(' <type-name> ')'
                  | '[' <type-name> ']'
                  | <ident>
                  | 'mut' <type-name>
<type-list>     ::= <type-name> { ',' <type-name> }
<comp-def>      ::= 'def' 'comp' <ident>  /\n+/
                        { <type-name> <ident> /\n+/ }
                    'end'
<rec-def>       ::= 'def' 'rec' <ident> /\n+/
                        { <ident> '(' [ <type-arg-list> ] ')' <type-name> /\n+/ }
                    'end'

<statement>     ::= <declaration> | <assignment> | <return> | <expr>
                  | <statement> ';' <statement>
<declaration>   ::= 'let' <type-name> <ident> [ '=' <expr> ]
<assignment>    ::= <ident> '=' <expr>
<return>        ::= 'return' <expr>

<expr>          ::= TBD | '(' <expr> ')'
```
