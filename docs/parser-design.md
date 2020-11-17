# Better Parsers

I've had to restart twice now because I want a parser that's nice to use.

Is it really too much to ask?????

## Parser Functions

I think it makes sense to define a parser as

```
Parser :: String -> (Token, String)
```

C++ resulted in a weird thing when trying to do a functional way, and had WAY too much clutter with if statements galore

Haskell looked prettier, but the "No exception/fail" thing was honestly a downside and it was *slow*

But assuming I still use this basic idea, how can I make a useful parser?

## C++ Version 2

So I need the speed. I like the parser thing.

I need an effective, clean, reusable way to implement it and combine it with others

So like

```
<member-acc>    ::= <ident> ':' ( <ident> | <member-acc> )
```

Should become something like

```
... memberAccess(...) {
    const auto name = apply(ident, input)
    const auto colon = combine(char(':'), name)
    const auto next = either(combine(ident, colon), combine(memberAcc, colon))
}
```

And this emulate the do notation
```
name <- ident
colon <- char ':'
next <- ident <|> memberAcc
```

But the problem is that do notation is supposed to emulate imperative code which C++ already is! So instead, I need something like:

```
createFrom({ ident, char(':'), either(ident, memberAcc) });
```

I tried something like this before though, and ran into issues trying to layer the functions together (because you can't pass state into lambdas and then still effectively pass the lambdas around)

So alternative idea using interfaces and objects instead:

```
typedef std::pair<Token, std::string> ParserResult;
struct Parser {
    virtual ParserResult parse(const std::string& input) const = 0;
};

struct SelectFrom : public Parser {
    const std::vector<Parser> options;
    ParserResult parse(const std::string& input) const override;
};

struct MemberAccess : public Parser {
    ParserResult parse(const std::string& input) const override;
};
```

etc.

I kinda want to do this in C# (don't worry performance would be there) as the header system is kind of annoying for this many repetitive things, but I also don't want to get stuck in the dotnet core system either, but eh. I'll just do the C++

## Switching to C#

The lack of true interfaces in C++ low-key sucks. I want to be able to pass a child in as a parent, not a pointer to a child in as a pointer to a parent.

Time to start over again. This time in C# (though using dotnet core native compilation)

It will basically be what I was already doing in the new C++ version
