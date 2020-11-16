#pragma once

#include <string>
#include <vector>
#include <utility>
#include <functional>

namespace good_basic {
    namespace parser_helpers {
        inline bool isDigit(const char c) {
            return c >= '0' && c <= '9';
        }

        inline bool isAlpha(const char c) {
            return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
        }
    }

    namespace parser {
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
        using Token = std::pair<TokenType, std::string>;
        using ParseResult = std::pair<Token, std::string>;
        using Parser = std::function<ParseResult(const std::string&)>;

        inline ParseResult parse(
                const Parser& parserFunc, const std::string& input) {
            return parserFunc(input);
        }

        Parser multiple(const Parser& parserFunc);
        Parser either(
            const Parser &parser1, const Parser& parser2
        );
        Parser selectFrom(const std::vector<Parser>& steps);
        Parser doParsers(const std::vector<Parser>& steps);

        extern const Parser alpha;
        extern const Parser digit;
        extern const Parser anyChar;
        Parser character(const char c);
        Parser anyCharExcept(const std::vector<char>& options);

        //extern const Parser expr;

        extern const Parser factor;
        /*extern const Parser memberAccess;
        extern const Parser funcCall;
        extern const Parser lambda;
        extern const Parser compOrRecDecl;*/

        extern const Parser ident;
        extern const Parser decimal;
        extern const Parser integer;
        extern const Parser str;
    }
}
