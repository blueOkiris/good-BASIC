# Grammar Definition

## What needs to be done

Okay so now we have a basic structure for things.

We know the data types and declarations
We know there will be function calls
We know there will be some kind of loop and some kind of if statement
We know there's some built in operators and functions

Now we need to define how all of that stuff actually works. So first let's define all of the specifics of those things

## What the language has

Okay so first data types:
1. Integers
2. Floating Point Numbers
3. Strings
4. Arrays
5. Function Types
6. Record Types
7. Composition Types

Declarations work like `let` + optional `mut` + type name + identifier + optional `=` + expression. For mutables, they can be reassigned with identifier + `=` + expression.

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
- I/O: print, input, write, read
