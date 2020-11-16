#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

// <conjunction> ::= <option> { '&&' <option> }
const Parser parser::conjunction = either(
    doParsers(
        {
            option, multiple(
                doParsers(
                    { character('&'), character('&'), option },
                    TokenType::Conjunction
                )
            )
        }, TokenType::Conjunction
    ), doParsers({ option }, TokenType::Conjunction)
);

// <option> ::= <term> { '||' <term> }
const Parser parser::option = either(
    doParsers(
        {
            term, multiple(
                doParsers(
                    { character('|'), character('|'), term }, TokenType::Option
                )
            )
        }, TokenType::Option
    ), doParsers({ term }, TokenType::Option)
);

// <term> ::= <factor> { ( '*' | '/' ) | <factor> }
const Parser parser::term = either(
    doParsers(
        {
            factor, multiple(
                doParsers(
                    { either(character('*'), character('-')), factor },
                    TokenType::Term
                )
            )
        }, TokenType::Term
    ), doParsers({ factor }, TokenType::Term)
);

/*
 * <factor> ::= <ident> | <int> | <float> | <string>
 *            | lambda | <comp-rec-dec>
 *            | <member-acc> | <func-call> | '(' <expr> ')'
 */
const Parser parser::factor = selectFrom(
    {
        ident, decimal, integer, str,
        lambda, compOrRecDecl,
        memberAccess, funcCall,
        //doParsers({ character('{'), expr, character('}') })*/
    }
);

// <member-acc> ::= <ident> ':' ( <ident> | <member-acc> )
const Parser parser::memberAccess = doParsers(
    { ident, character(':'), either(memberAccess, ident) },
    TokenType::MemberAccess
);

// <func-call> ::= 'call' <ident> { <expr> }
const Parser parser::funcCall = [](const std::string& input) {
    const std::vector<Parser> steps = {
        character('c'), character('a'), character('l'), character('l'), ident
    };
    const std::vector<Parser> stepsWExpr = {
        character('c'), character('a'), character('l'), character('l'), ident,
        //expr
    };
    return parse(
        either(
            doParsers(stepsWExpr, TokenType::FuncCall),
            doParsers(steps, TokenType::FuncCall)
        ), input
    );
};

/*
 * <lambda> ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name> /\n+/
 *                  { <statement> /\n+/ }
 *              'end'
 */
const Parser parser::lambda = doParsers(
    {
        character('l'), character('a'), character('m'), character('b'),
        character('d'), character('a'),
        character('('), /*typeArgList,*/ character(')'), //character('\n'),
        multiple(
            doParsers(
                { /*statement,*/ /*character('\n')*/ }, TokenType::Statement
            )
        ),
        character('e'), character('n'), character('d')
    }, TokenType::Lambda
);

// <comp-rec-dec> ::= 'data' <ident> '(' [ <expr> { ',' <expr> } ] ')'
const Parser parser::compOrRecDecl = doParsers(
    {
        character('d'), character('a'), character('t'), character('a'),
        ident, character('('),
        /*either(
            doParsers(
                {
                    expr, multiple(
                        doParsers( { character(','), expr }, TokenType::Expr)
                    )
                }, TokenType::Expr
            ), expr
        ),*/
        character(')')
    }, TokenType::CompOrRecDec
);

// <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
const Parser parser::ident = [](const std::string &input) {
    const auto firstChar = either(alpha, character('_'));
    const auto laterChars = selectFrom({ alpha, character('_'), digit });
    return parse(
        either(
            doParsers({ firstChar, multiple(laterChars) }, TokenType::Ident),
            doParsers({ firstChar }, TokenType::Ident)
        ), input
    );
};

// <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
const Parser parser::decimal = [](const std::string &input) {
    const std::vector<Parser> nonNegSteps = {
        either(
            either(
                doParsers(
                    { multiple(digit), character('.'), multiple(digit) },
                    TokenType::Float
                ), doParsers(
                    { multiple(digit), character('.') }, TokenType::Float
                )
            ), doParsers({ character('.'), multiple(digit) }, TokenType::Float)
        )
    };
    const auto basicNumber = either(
        doParsers(
            { character('-'), doParsers(nonNegSteps, TokenType::Int) },
            TokenType::Float
        ), doParsers(nonNegSteps, TokenType::Int)
    );

    const auto scienceNumber = either(
        doParsers({ basicNumber, character('e'), integer }, TokenType::Float),
        basicNumber
    );
    return parse(scienceNumber, input);
};

// <int> ::= /-?[0-9]+/
const Parser parser::integer = either(
    doParsers(
        { either(character('-'), character('+')), multiple(digit) },
        TokenType::Int
    ), multiple(digit)
);

// <string> ::= /'(\\.|[^\\'])*'/
const Parser parser::str = doParsers(
    {
        character('\''),
        either(
            doParsers( // Non-empty string
                {
                    multiple(
                        either(
                            doParsers(
                                { character('\\'), anyChar },
                                TokenType::Character
                            ), anyCharExcept({ '\'', '\\' })
                        )
                    ), character('\'')
                }, TokenType::String
            ), character('\'') // Empty string
        )
    }, TokenType::String
);
