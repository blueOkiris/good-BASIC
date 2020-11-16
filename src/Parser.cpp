#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <sstream>
#include <iostream>
#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

std::string Token::str() const {
    std::stringstream output;
    output << "{ " << static_cast<int>(type) << ", " << source;
    if(children.size() > 0) {
        output << ", { ";
        for(const auto& token : children) {
            output << token.str();
            output << " ";
        }
        output << "}";
    }
    output << " }";
    return output.str();
}

const Parser parser::digit = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isDigit(input[0])) {
        return ParseResult(
            { { TokenType::None, "", std::vector<Token>() }, input }
        );
    } else {
        return ParseResult(
            {
                {
                    TokenType::Int, input.substr(0, 1),
                    std::vector<Token>()
                }, input.substr(1)
            }
        );
    }
};

const Parser parser::alpha = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isAlpha(input[0])) {
        return ParseResult(
            { { TokenType::None, "", std::vector<Token>() }, input }
        );
    } else {
        return ParseResult(
            {
                {
                    TokenType::Character, input.substr(0, 1),
                    std::vector<Token>()
                }, input.substr(1)
            }
        );
    }
};

const Parser parser::anyChar = [](const std::string& input) {
    if(input.length() < 1) {
        return ParseResult(
            { { TokenType::None, "", std::vector<Token>() }, input }
        );
    } else {
        return ParseResult(
            {
                {
                    TokenType::Character, input.substr(0, 1),
                    std::vector<Token>()
                }, input.substr(1)
            }
        );
    }
};

Parser parser::character(const char c) {
    return [c](const std::string& input) {
        if(input.length() < 1 || input[0] != c) {
            return ParseResult(
                { { TokenType::None, "", std::vector<Token>() }, input }
            );
        } else {
            return ParseResult(
                {
                    {
                        TokenType::Character, input.substr(0, 1),
                        std::vector<Token>()
                    }, input.substr(1)
                }
            );
        }
    };
}

Parser parser::anyCharExcept(const std::vector<char>& options) {
    return [options](const std::string& input) {
        if(input.length() < 1) {
            return ParseResult({ { TokenType::None, "" }, input });
        } else {
            for(const char c : options) {
                if(input[0] == c) {
                    return ParseResult(
                        { { TokenType::None, "", std::vector<Token>() }, input }
                    );
                }
            }
            return ParseResult(
                {
                    {
                        TokenType::Character, input.substr(0, 1),
                        std::vector<Token>()
                    }, input.substr(1)
                }
            );
        }
    };
}

Parser parser::multiple(const Parser& parserFunc) {
    return [parserFunc](const std::string& input) {
        std::stringstream success;
        auto result = parse(parserFunc, input);
        TokenType type = result.first.type;
        while(result.first.type != TokenType::None) {
            success << result.first.source;
            result = parse(parserFunc, result.second);
        }
        return ParseResult({ { type, success.str() }, result.second });
    };
}

Parser parser::either(const Parser& parser1, const Parser& parser2) {
    return [parser1, parser2](const std::string& input) {
        const auto try1 = parse(parser1, input);
        if(try1.first.type == TokenType::None) {
            return parse(parser2, input);
        } else {
            return try1;
        }
    };
}

Parser parser::selectFrom(const std::vector<Parser>& options) {
    return [options](const std::string& input) {
        for(const auto& option : options) {
            const auto attempt = parse(option, input);
            if(attempt.first.type != TokenType::None) {
                return attempt;
            }
        }
        return ParseResult({ { TokenType::None, "" }, input });
    };
}

Parser parser::doParsers(
        const std::vector<Parser>& steps, const TokenType type) {
    return [steps, type](const std::string& input) {
        auto currInput = input;
        std::stringstream finalParse;
        std::vector<Token> children;
        for(const auto& parserFunc : steps) {
            const auto result = parse(parserFunc, currInput);
            if(result.first.type == TokenType::None) {
                return ParseResult({ { TokenType::None, "" }, input });
            }
            children.push_back(result.first);
            finalParse << result.first.source;
            currInput = result.second;
        }
        return ParseResult({ { type, finalParse.str(), children }, currInput });
    };
}

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

/*
 * <factor> ::= <ident> | <int> | <float> | <string>
 *            | lambda | <comp-rec-dec>
 *            | <member-acc> | <func-call> | '(' <expr> ')'
 */
const Parser parser::factor = selectFrom(
    {
        ident, decimal, integer, str,
        /*lambda, compOrRecDecl,
        memberAccess, funcCall,
        doParsers({ character('{'), expr, character('}') })*/
    }
);

// <member-acc> ::= <ident> ':' ( <ident> | <member-acc> )
const Parser parser::memberAccess = doParsers(
    { ident, character(':'), either(memberAccess, ident) },
    TokenType::MemberAccess
);
