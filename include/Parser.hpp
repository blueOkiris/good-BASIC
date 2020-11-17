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
        TokenType type;
        std::string source;
        std::vector<Token> children;
        
        static Token pair(
            const Token& a, const Token& b,
            const TokenType compType = TokenType::None
        );
        std::string str() const;
    };
    
    typedef std::pair<Token, std::string> ParserResult;
    struct Parser {
        virtual std::vector<TokenType> type() const = 0;
        virtual ParserResult parse(const std::string& input) const = 0;
    };

    struct SelectFrom : public Parser {
        std::vector<Parser> options;
        std::vector<TokenType> type() const override;
        ParserResult parse(const std::string& input) const override;
    };

    struct CreateFrom : public Parser {
        std::vector<Parser> steps;
        TokenType resultType;
        std::vector<TokenType> type() const override;
        ParserResult parse(const std::string& input) const override;
    };
    
    struct ParserException : public std::exception {
        private:
            const std::string _message;
        
        public:
            ParserException(const std::string& message);
            const char* what() const throw();
    };
    
    struct UnexpectedTokenException : public ParserException {
        UnexpectedTokenException(const std::vector<TokenType>& expectedTypes);
    };
}
