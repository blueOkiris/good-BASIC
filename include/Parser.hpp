#pragma once

#include <vector>
#include <string>
#include <memory>
#include <exception>

namespace good_basic {
    enum class TokenType {
        Module, Import, Export, Implement, IdentList,
        Definition, FuncDef, TypeArgList, TypeName, CompDef, RecDef,
        Statement, Declaration, Assignment, Return,
        Expr, Product, Summation, Shift, Inequality, Equality, MaskOff,
        Exclusive, MaskOn, Conjunction, Option, Term,
        Factor, MemberAccess, FuncCall, Lambda, CompOrRecDec,
        Ident, Float, Int, String, Character, Digit,
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
    class Parser {
        public:
            virtual std::vector<TokenType> type() const = 0;
            virtual ParserResult parse(const std::string& input) const = 0;
    };

    class SelectFrom : public Parser {
        private:
            std::vector<std::shared_ptr<Parser>> _options;
            
        public:
            SelectFrom(const std::vector<std::shared_ptr<Parser>>& options);
            std::vector<TokenType> type() const override;
            ParserResult parse(const std::string& input) const override;
    };

    class CreateFrom : public Parser {
        private:
            std::vector<std::shared_ptr<Parser>> _steps;
            TokenType _resultType;
            
        public:
            CreateFrom(
                const std::vector<std::shared_ptr<Parser>>& steps,
                const TokenType resultType
            );
            std::vector<TokenType> type() const override;
            ParserResult parse(const std::string& input) const override;
    };
    
    class Many : public Parser {
        private:
            std::shared_ptr<Parser> _what;
            
        public:
            Many(const std::shared_ptr<Parser>& what);
            std::vector<TokenType> type() const override;
            ParserResult parse(const std::string& input) const override;
    };
    
    class Char : public Parser {
        private:
            char _c;
            
        public:
            Char(const char c);
            std::vector<TokenType> type() const override;
            ParserResult parse(const std::string& input) const override;
    };
    
    class Digit : public Parser {
        public:
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
