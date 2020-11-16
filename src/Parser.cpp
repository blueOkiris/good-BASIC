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
