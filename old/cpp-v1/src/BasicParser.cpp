#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

// <expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
const Parser parser::expr = selectFrom(
    {
        doParsers(
            {
                either(character('!'), character('~')),
                product,
                either(
                    doParsers(
                        { character('+'), character('+') }, TokenType::Expr
                    ), doParsers(
                        { character('-'), character('-') }, TokenType::Expr
                    )
                )
            }, TokenType::Expr
        ), doParsers(
            {
                product,
                either(
                    doParsers(
                        { character('+'), character('+') }, TokenType::Expr
                    ), doParsers(
                        { character('-'), character('-') }, TokenType::Expr
                    )
                )
            }, TokenType::Expr
        ), doParsers(
            {
                either(character('!'), character('~')),
                product
            }, TokenType::Expr
        ), doParsers({ product }, TokenType::Expr)
    }
);

// <product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
const Parser parser::product = either(
    doParsers(
        {
            summation, multiple(
                doParsers(
                    {
                        selectFrom(
                            { character('*'), character('/'), character('%') }
                        ), summation 
                    }, TokenType::Product
                )
            )
        }, TokenType::Product
    ), doParsers({ summation }, TokenType::Product)
);

// <summation> ::= <shift> { ( '+' | '-' ) <shift> }
const Parser parser::summation = either(
    doParsers(
        {
            shift, multiple(
                doParsers(
                    { either(character('+'), character('-')), shift },
                    TokenType::Summation
                )
            )
        }, TokenType::Summation
    ), doParsers({ shift }, TokenType::Summation)
);

// <shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
const Parser parser::shift = either(
    doParsers(
        {
            inequality, multiple(
                doParsers(
                    {
                        either(
                            doParsers(
                                { character('<'), character('<') },
                                TokenType::Character
                            ), doParsers(
                                { character('>'), character('>') },
                                TokenType::Character
                            )
                        ), inequality
                    },
                    TokenType::Shift
                )
            )
        }, TokenType::Shift
    ), doParsers({ inequality }, TokenType::Shift)
);

// <inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
const Parser parser::inequality = either(
    doParsers(
        {
            equality, multiple(
                doParsers(
                    {
                        selectFrom(
                            {
                                character('<'), character('>'),
                                doParsers(
                                    { character('<'), character('=') },
                                    TokenType::Character
                                ), doParsers(
                                    { character('>'), character('=') },
                                    TokenType::Character
                                )
                            }
                        ), equality
                    },
                    TokenType::Inequality
                )
            )
        }, TokenType::Inequality
    ), doParsers({ equality }, TokenType::Inequality)
);

// <equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
const Parser parser::equality = either(
    doParsers(
        {
            maskOff, multiple(
                doParsers(
                    {
                        either(
                            doParsers(
                                { character('='), character('=') },
                                TokenType::Character
                            ), doParsers(
                                { character('!'), character('=') },
                                TokenType::Character
                            )
                        ), maskOff
                    },
                    TokenType::Equality
                )
            )
        }, TokenType::Equality
    ), doParsers({ maskOff }, TokenType::Equality)
);

// <mask-off> ::= <exclusive> { '&' <exclusive> }
const Parser parser::maskOff = either(
    doParsers(
        {
            exclusive, multiple(
                doParsers(
                    { character('&'), exclusive },
                    TokenType::MaskOff
                )
            )
        }, TokenType::MaskOff
    ), doParsers({ exclusive }, TokenType::MaskOff)
);

// <exclusive> ::= <mask-on> { '^' <mask-on> }
const Parser parser::exclusive = either(
    doParsers(
        {
            maskOn, multiple(
                doParsers(
                    { character('^'), maskOn },
                    TokenType::Exclusive
                )
            )
        }, TokenType::Exclusive
    ), doParsers({ maskOn }, TokenType::Exclusive)
);

// <mask-on> ::= <conjunction> { '|' <conjunction> }
const Parser parser::maskOn = either(
    doParsers(
        {
            conjunction, multiple(
                doParsers(
                    { character('|'), conjunction },
                    TokenType::MaskOn
                )
            )
        }, TokenType::MaskOn
    ), doParsers({ conjunction }, TokenType::MaskOn)
);

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
            factor, multiple(
                doParsers(
                    { character('|'), character('|'), factor },
                    TokenType::Option
                )
            )
        }, TokenType::Option
    ), doParsers({ factor }, TokenType::Option)
);

/*
 * <factor> ::= <ident> | <int> | <float> | <string>
 *            | lambda | <comp-rec-dec>
 *            | <member-acc> | <func-call> | '(' <expr> ')'
 */
const Parser parser::factor = [](const std::string &input) {
    return parse(
        doParsers(
            {
                selectFrom(
                    {
                        ident, decimal, integer, str,
                        lambda, compOrRecDecl,
                        memberAccess, funcCall,
                        doParsers(
                            { character('{'), expr, character('}') },
                            TokenType::Factor
                        )
                    }
                )
            }, TokenType::Factor
        ), input
    );
};

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
        expr
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
        either(
            doParsers(
                {
                    expr, multiple(
                        doParsers( { character(','), expr }, TokenType::Expr)
                    )
                }, TokenType::Expr
            ), expr
        ),
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
const Parser parser::integer = [](const std::string& input) {
    return parse(
        either(
            doParsers(
                { either(character('-'), character('+')), multiple(digit) },
                TokenType::Int
            ), multiple(digit)
        ), input
    );
};

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
