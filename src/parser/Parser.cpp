#include <vector>
#include <exception>
#include <string>
#include <sstream>
#include <memory>
#include <parser/Parser.hpp>

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

inline std::vector<Token> combine(
        const std::vector<Token>& a, const std::vector<Token>& b) {
    std::vector<Token> compound(a);
    for(const auto& token : b) {
        compound.push_back(token);
    }
    return compound;
}

Token Token::pair(const Token& a, const Token& b, const TokenType compType) {
    if(a.type == b.type || a.type == compType) {
        return { a.type, a.source + b.source, combine(a.children, b.children) };
    } else if(b.type == compType) {
        return { b.type, a.source + b.source, combine(a.children, b.children) };
    } else {
        return { compType, a.source + b.source, std::vector<Token>({ a, b }) };
    }
}

SelectFrom::SelectFrom(const std::vector<std::shared_ptr<Parser>>& options) :
        _options(options) {
}

ParserResult SelectFrom::parse(const std::string& input) const {
    for(const auto& parser : _options) {
        try {
            const auto token = parser->parse(input);
            return token;
        } catch(const UnexpectedTokenException& ute) {
            continue;
        }
    }
    throw UnexpectedTokenException(type());
}

std::vector<TokenType> SelectFrom::type() const {
    std::vector<TokenType> typeList;
    for(const auto& parser : _options) {
        const auto types = parser->type();
        for(const auto& type : types) {
            typeList.push_back(type);
        }
    }
    return typeList;
}

CreateFrom::CreateFrom(
        const std::vector<std::shared_ptr<Parser>>& steps,
        const TokenType resultType) :
        _steps(steps), _resultType(resultType) {
}

ParserResult CreateFrom::parse(const std::string& input) const {
    auto currInp = input;
    Token finalToken = { TokenType::None, "", std::vector<Token>() };
    for(const auto& parser : _steps) {
        const auto result = parser->parse(currInp);
        if(finalToken.type == TokenType::None) {
            finalToken = result.first;
        } else {
            finalToken = Token::pair(finalToken, result.first);
        }
        currInp = result.second;
    }
    return { { _resultType, finalToken.source, finalToken.children }, currInp };
}

std::vector<TokenType> CreateFrom::type() const {
    return { _resultType };
}

Many::Many(const std::shared_ptr<Parser>& what) : _what(what) {
}

std::vector<TokenType> Many::type() const {
    return _what->type();
}

ParserResult Many::parse(const std::string& input) const {
    auto result = _what->parse(input); // Outside of loop so it fails if none
    
    auto finalToke = result.first;
    auto currInp = result.second;
    
    bool quit = false;
    while(!quit) {
        try {
            result = _what->parse(currInp);
            finalToke = Token::pair(finalToke, result.first);
            currInp = result.second;
        } catch(const UnexpectedTokenException& ute) {
            quit = true;
        }
    }
    
    return { finalToke, currInp };
}

Char::Char(const char c) : _c(c) {
}

std::vector<TokenType> Char::type() const {
    return { TokenType::Character };
}

ParserResult Char::parse(const std::string& input) const {
    if(input.length() < 1 || input[0] != _c) {
        throw UnexpectedTokenException(type());
    }
    return {
        { TokenType::Character, input.substr(0, 1), std::vector<Token>() },
        input.substr(1) 
    };
}

std::vector<TokenType> Digit::type() const {
    return { TokenType::Digit };
}

ParserResult Digit::parse(const std::string& input) const {
    if(input.length() < 1 || (input[0] < '0' || input[0] > '9')) {
        throw UnexpectedTokenException(type());
    }
    return {
        { TokenType::Digit, input.substr(0, 1), std::vector<Token>() },
        input.substr(1) 
    };
}

ParserException::ParserException(const std::string& message) :
        _message(message) {
}

const char* ParserException::what() const throw() {
    return _message.c_str();
}

inline std::string typesToExceptionStr(const std::vector<TokenType>& expTypes) {
    std::stringstream messageStr;
    for(const auto& type : expTypes) {
        messageStr << static_cast<int>(type) << " ";
    }
    return messageStr.str();
}

UnexpectedTokenException::UnexpectedTokenException(
        const std::vector<TokenType>& expecTypes) :
        ParserException(
            "Parser Exception: Unexpected token. Expected tokens: "
            + typesToExceptionStr(expecTypes)
        ) {
}
