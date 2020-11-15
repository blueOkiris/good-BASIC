#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <sstream>
#include <iostream>
#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

Parser parser::doParsers(const std::vector<Parser>& steps) {
    return [steps](const std::string& input) {
        auto currInput = input;
        std::stringstream finalParse;
        for(const auto& parserFunc : steps) {
            const auto result = parse(parserFunc, currInput);
            if(result.first == "") {
                return ParseResult({ "", input });
            }
            finalParse << result.first;
            currInput = result.second;
        }
        return ParseResult({ finalParse.str(), currInput });
    };
}

const Parser parser::digit = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isDigit(input[0])) {
        return ParseResult({ "", input });
    } else {
        return ParseResult({ input.substr(0, 1), input.substr(1) });
    }
};

const Parser parser::alpha = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isAlpha(input[0])) {
        return ParseResult({ "", input });
    } else {
        return ParseResult({ input.substr(0, 1), input.substr(1) });
    }
};

const Parser parser::anyChar = [](const std::string& input) {
    if(input.length() < 1) {
        return ParseResult({ "", input });
    } else {
        return ParseResult({ input.substr(0, 1), input.substr(1) });
    }
};

Parser parser::character(const char c) {
    return [c](const std::string& input) {
        if(input.length() < 1 || input[0] != c) {
            return ParseResult({ "", input });
        } else {
            return ParseResult({ input.substr(0, 1), input.substr(1) });
        }
    };
}

Parser parser::anyCharExcept(const std::vector<char>& options) {
    return [options](const std::string& input) {
        if(input.length() < 1) {
            return ParseResult({ "", input });
        } else {
            for(const char c : options) {
                if(input[0] == c) {
                    return ParseResult({ "", input });
                }
            }
            return ParseResult({ input.substr(0, 1), input.substr(1) });
        }
    };
}

Parser parser::multiple(const Parser& parserFunc) {
    return [parserFunc](const std::string& input) {
        std::stringstream success;
        auto result = parse(parserFunc, input);
        while(result.first != "") {
            success << result.first;
            result = parse(parserFunc, result.second);
        }
        return ParseResult({ success.str(), result.second });
    };
}

Parser parser::either(const Parser& parser1, const Parser& parser2) {
    return [parser1, parser2](const std::string& input) {
        const auto try1 = parse(parser1, input);
        if(try1.first == "") {
            return parse(parser2, input);
        } else {
            return try1;
        }
    };
}

// <int> ::= /-?[0-9]+/
const Parser parser::integer = [](const std::string &input) {
    return parse(
        either(
            doParsers(
                { either(character('-'), character('+')), multiple(digit) }
            ), multiple(digit)
        ), input
    );
};

// <string> ::= /'(\\.|[^\\'])*'/
const Parser parser::str = [](const std::string &input) {
    const std::vector<Parser> steps = {
        character('\''),
        either(
            doParsers( // Non-empty string
                {
                    multiple(
                        either(
                            doParsers({ character('\\'), anyChar }),
                            anyCharExcept({ '\'', '\\' })
                        )
                    ), character('\'')
                }
            ), character('\'') // Empty string
        )
    };
    return parse(doParsers(steps), input);
};

// <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
const Parser parser::decimal = [](const std::string &input) {
    const std::vector<Parser> nonNegSteps = {
        either(
            either(
                doParsers({ multiple(digit), character('.'), multiple(digit) }),
                doParsers({ multiple(digit), character('.') })
            ), doParsers({ character('.'), multiple(digit) })
        )
    };
    const auto basicNumber = either(
        doParsers({ character('-'), doParsers(nonNegSteps) }),
        doParsers(nonNegSteps)
    );

    const auto scienceNumber = either(
        doParsers({ basicNumber, character('e'), integer }),
        basicNumber
    );
    return parse(scienceNumber, input);
};

// <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
const Parser parser::ident = [](const std::string &input) {
    const auto firstChar = either(alpha, character('_'));
    const auto laterChars = either(either(alpha, character('_')), digit);
    return parse(
        either(
            doParsers({ firstChar, multiple(laterChars) }),
            firstChar
        ), input
    );
};
