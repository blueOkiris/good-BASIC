# Good BASIC

## Description and Goals

A lot of people will say BASIC is bad, but why is it bad, and does it have to *be* bad? I don't think so, so I'm making a BASIC that aims to be a better language.

How do I define good language? In my opinion, a good language causes you to write good code by default, or at least tries to. Beyond that it should be capable of doing powerful things. I'm trying to give those two aspects to basic.

In [this other document](./docs/better-basic.md), I describe my process to design a better BASIC language, but essentially it is built on four principle rules:

1. Composition over Inheritance
2. Modules over Encapsulation
3. 1st Class Functions over Methods
4. Data and Record Types over Objects

The reasoning for these is somewhat explained in said document

## Example

I think the best way to understand why you might want a different language is to see the language in action. Here's an example that shows some of the features of the language.

File - MyInterface .bb:
```
export MyInterface1

def comp MyInterface1
    printHelloWorld() Void
    printWhatever(str input) Void
end
```

File - Concrete .bb
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

File - TestProgram .bb
```
Imports MyInterface
IMPORTS Concrete
imports std

DEF Fn callPrints(MYINTERFACE1 interface)
    CALL interface:PRINTHelloWorld
    call interface:printWhatever 'Hello, there!'
end

def fn main([STR] args)
    call callPrints Concrete
    let int two = call Concrete:extraFunc 2
    print call std:toStr two
end
```

## Building

Currently, there is no source, but when there is, I'll probably use a Makefile, so to build the compiler: `make`
