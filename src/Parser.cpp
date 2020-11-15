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

const Parser parser::character(const char c) {
    return [c](const std::string& input) {
        if(input.length() < 1 || input[0] != c) {
            return ParseResult({ "", input });
        } else {
            return ParseResult({ input.substr(0, 1), input.substr(1) });
        }
    };
}

const Parser parser::any = [](const std::string& input) {
    if(input.length() < 1) {
        return ParseResult({ "", input });
    } else {
        return ParseResult({ input.substr(0, 1), input.substr(1) });
    }
};

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
            doParsers({ character('-'), multiple(digit) }),
            multiple(digit)
        ), input
    );
};

// <string> ::= /'(\\.|[^\\'])*'/
const Parser parser::str = [](const std::string &input) {
    std::stringstream parsedStr;

    const auto quote = parse(character('\''), input);
    if(quote.first == "") {
        return ParseResult({ "", input });
    }
    parsedStr << quote.first;

    auto currInput = quote.second;
    while(currInput != "") {
        const auto escape = parse(character('\\'), currInput);
        if(escape.first != "") {
            parsedStr << escape.first;
            currInput = escape.second;
            if(currInput == "") {
                return ParseResult({ "", input });
            }

            parsedStr << currInput[0];
            currInput = currInput.substr(1);
            continue;
        }
        
        const auto endQuote = parse(character('\''), currInput);
        if(endQuote.first != "") {
            parsedStr << endQuote.first;
            currInput = endQuote.second;
            return ParseResult({ parsedStr.str(), currInput });
        }

        parsedStr << currInput[0];
        currInput = currInput.substr(1);
    }
    return ParseResult({ "", input });
};
