# Building a Better BASIC

Basic is neat. It is this very weird way to program. I'm talking older BASICs, not like Visual Basic or something

It's vaguely C-like, but not really. It's also usually interpreted. The thing that makes it interesting how pervasive it was. It seemed that anyone with a remote interest in programming could pick up BASIC on their old machine and learn, very much the role filled by Python today.

BASIC is not good though. Let's talk about why

## Why BASIC isn't good

People can find many reasons why they don't like BASIC (I'm talking about language/syntax here, not just like "oh it's slow" as someone *could* make a highly efficient BASIC compiler nowadays or something, even though speed is arguably one of the reasons it disappeared), but I want to list out and define several major issues with the design. I'll take some from pre-structured BASIC as well as modern iterations like VisualBASIC

These aren't necessarily my opinion, just some things I found online

1. Old basics didn't have any kind of pointer system really, had GOTO, no if/else nesting, and used line numbers. Many modern iterations don't, but it's something to take note of on what not to do

2. BASIC is not OO, and while it doesn't *need* to be, not having some sort of containment for data or modules is an issue. Again, VisualBASIC does this okay

3. Poor error handling

4. Even modern VB suffers from lack of separation of concerns, despite having OO

5. Tends to lead to lots of global mutable state and other bad programming practices

6. Not enough syntax-sugar. For instance can't declare variables and assign to them at the same time

7. Lack of good polymorphism

8. Not a good set of libraries; most likely due to the single-file style devlopment that it advocates

## Improvements

Okay, so how do we solve some of these issues?

*Functional Programming*

Okay I'm kidding, partially. BASIC stands for Beginner's All-Purpose Symbolic Instruction Code. It wouldn't make sense if that wasn't imperative

But what I'm saying is bring in some features that modern functional programming languages tend to do well, but none of them actually have to do with functional programming. They are a consequence of not being OO. What are these features?

Well, most people are familiar with Composition over Inheritance, and I'd like to propose 4 rules in that same vein for "good" programming.

1. Composition over Inheritance
2. Modules over Encapsulation
3. 1st Class Functions over Methods
4. Data and Record Types over Objects

These rules are still very much arguable as to their usefulness, but you can find many people arguing and explaining why these would be good things.

Essentially, the argument goes something along these lines.

1. Composition over Inheritance is good for reusability and change.

2. Modules are more applicable than simply Encapsulation which is why so many modern OOP languages are adopting it along with OOP

3. With Modules, there's not really as much of a need to contain functions within Objects, and first class functions have many more uses

4. And without methods, Objects are really just mutable Record types, and many people would argue in favor of immutability by default

With those established, BASIC can actually fit these fairly easily, and you'll see I mean, but first we need to establish what exactly I mean when I say BASIC

## Traditional Instruction Sets

Most of this is taken from [Wikipedia](https://en.wikipedia.org/wiki/BASIC)

__Data Manipulation__
- `LET` assigns a value to a variable
- `DATA` holds a list of values assigned sequentially with `READ`
- `READ` takes a value from `DATA`, assigns it to a variable, and keeps track of the location using a pointer. I found these two confusing, so [here is a link explaining](http://www.math.hawaii.edu/~hile/basic/basic4.htm)
- `RESTORE` resets the internal pointer

__Program Flow Control__
- `IF` ... `THEN` ... `ELSE` ... `END` work like other languages
- `FOR` ... `TO` ... `STEP` ... `NEXT` ... `END` given a variable with an assignment with a given end condition with a given step (default of 1) loop
- `WHILE` ... `END` and `REPEAT` ... `UNTIL` loops until a condition is met
- `GOTO` and `GOSUB` jumps to a label or jumps to a label and expects return
- `ON` ... `GOTO` jumps if there's a condition
- `DEF FN` for fortran like functions

__I/O__
- `LIST` displays source code
- `PRINT` prints a message to screen
- `INPUT` asks user to input a variable optionally with a prompt
- `TAB` sets screen position also sometimes `AT`
- `SPC` prints out a number of space characters

__Mathematical Functions__
- `ABS` absolute value
- `ATN` arctangent
- `COS` cosine
- `EXP` exponential
- `INT` integer part of a float
- `LOG` __natural__ log
- `RND` random number generation
- `SIN` sine
- `SQR` square __root__
- `TAN` tangent

__Miscellaneous__
- `REM`
- `USR` like assembly or machine code sub routine call
- `CALL` like usr
- `TRON` debugging with line numbers on
- `ASM` inline assembly

__Data Types__

BASIC could be considered to support integer types, string types, floating points, and arrays

## Designing the new BASIC

So let's recall our four rules:
1. Composition over Inheritance
2. Modules over Encapsulation
3. First Class Functions over Methods
4. Data and Record Types over Objects

I think it will be easier if we start from rule 4 and work our way up.

__Data and Record Types over Objects__
So obviously not OO, but record types?
How can BASIC have those?

Note: although many iterations of BASIC have had limited variables, I don't think there's a need for that here just the typical C way of variable names /[A-Za-z_][A-Za-z0-9_]/. I also think case-sensitivity is good in general, but I understand why it should be in basic. Beginner's *do* often mistype a variable by case, and so since it's been in there forever, case insensitivty will remain.

I propose this method for defining record types:

```
DEF REC Person
    STR name
    INT age
END
```

I think that's a very BASIC-like way to make a record type. For those unfamiliar, a record type is like a C-struct

So now that that's settled, let's define our data types:
1. `INT` - integer types. No char or short or long or whatever though, just int-size integers. There's reasons for this, but I won't get into it
2. `FLOAT` - ACTUALLY a double because I'm assuming you're using a modern PC
3. `STR` - A string. FYI I'm assuming standard library stuff to convert a string of length 1 to the ASCII value i.e. casting a char to an integer
4. Custom record types ala above
5. `[...]` - arrays of any type. Specific library functions will be created to manipulate them them, including declaration, so I won't touch them here really

For declaring variables, I'll take a note out of Rust's book. `LET` declares a constant and `LET MUT` declares a variable. I think I'll also have static typing as it's just better. It'll catch more issues at compile time. If you want a value that will be a mutable string you'd say: `LET MUT STR MyString` which is wordy, but feels very "BASICy" to me. Obviously you should default to no `MUT`

Also I want assignment in declarations as it's just normal for languages at this point in time: `let mut str mystring = 'Hello, world!\n'`

I won't make the language whitespace-bad i.e. tab/space-based as that's pretty awful, *Python*, but I will make it line-based with an optional semi-colon used to place multiple statements on a single line

__First Class Functions over Methods__
Okay, so I'll ammend my types now:
6. Functions

How to declare functions?

```
Def Fn Sum(Int x, Int y) Int
    RETURN X + Y
End
```

This actually declares a function-type constant. An alternative way is a lambda expression

```
Let Fn(Int, Int) Int Sum = LAMBDA(INT X, INT Y) Int
    RETURN X + Y
END
```

Note that a newline or a semicolon is required after `LAMBDA(INT, INT) Int` above

And all of that means you can do stuff like:

```
Def Fn Sum(Int x, Int y) Int
    RETURN X + Y
End
Let FN(INT, INT) INT sum2 = Sum
Let FN(FN(INT, INT)) INT callSumWith2And2 = lambda(FN(INT, INT) INT function) INT
    RETURN CALL FUNCTION 2 2
END
```

__Modules over Encapsulation__

I think a simple and useful way to do modules is just to have each file be a module.

In the module file, the first line can just be: `EXPORTS CONST FUNC1, CONST FUNC2, ...`

Yes that's right, you can only export constant function types from a module. There's reasons for this, but essentially I think it will lead to better code when calling a module.

__Composition over Inheritance__

Okay, so how do we define an Interface?
Quite simply, it's a record type with just functions, so the syntax should be similar.

```
DEF COMP
    Function1() Int
    Function2(Int a, Int b) STR
    ...
END
```

I called it a composition so that the word is not `Int` which is take or `Interface` which is long

You'd be able to export this as well, but here's the final part. A module can implement an interface

So at the top you'd have: `EXPORTS ... IMPLENTS ...`

Then you'd be required to implement those functions. Finally, we must once again ammend our type list:

7. `COMP` - Interfaces. Work like Record Types but only contain functions.

This allows you to pass in entire modules into functions:

MyInterface .bb:
```
export MyInterface1

def comp MyInterface1
    printHelloWorld() Void
    printWhatever(str) Void
end
```

Concrete .bb
```
imports MyInterface
exports printHelloWorld, printWhatever, extraFunc implements MyInterface:MyInterface1

def fn printHelloWorld() Void
    print 'Hello, world!\n'
end

def fn printWhatever(str input) Void
    print input
end

def fn extraFunc(int input)
    return input
end
```

TestProgream .bb
```
imports MyInterface
imports Concrete
imports std

def fn callPrints(MyInterface1 interface)
    call interface:printHelloWorld
    call interface:printWhatever 'Hello, there!'
end

def fn main([str] args)
    call callPrints Concrete
    let int two = call Concrete:extraFunc 2
    print call std:toStr two
end
```

Yeah. I think that's a good start
