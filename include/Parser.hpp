#pragma once

#include <vector>
#include <string>
#include <exception>

namespace good_basic {
    enum class TokenType {
        Module, Import, Export, Implement, IdentList,
        Definition, FuncDef, TypeArgList, TypeName, CompDef, RecDef,
        Statement, Declaration, Assignment, Return,
        Expr, Product, Summation, Shift, Inequality, Equality, MaskOff,
        Exclusive, MaskOn, Conjunction, Option, Term,
        Factor, MemberAccess, FuncCall, Lambda, CompOrRecDec,
        Ident, Float, Int, String, Character,
        None
    };
    struct Token {
        const TokenType type;
        const std::string source;
        const std::vector<Token> children;
        std::string str() const;
    };
    
    typedef std::pair<Token, std::string> ParserResult;
    struct Parser {
        virtual ParserResult parse(const std::string& input) const = 0;
    };

    struct SelectFrom : public Parser {
        const std::vector<Parser> options;
        ParserResult parse(const std::string& input) const override;
    };

    struct CreateFrom : public Parser {
        const std::vector<Parser> steps;
        ParserResult parse(const std::string& input) const override;
    };
}
