#include <vector>
#include <exception>
#include <string>
#include <sstream>
#include <Parser.hpp>

using namespace good_basic;

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

Token Token::pair(const Token& a, const Token& b) {
    return { TokenType::None, "", std::vector<Token>() };
}

ParserResult SelectFrom::parse(const std::string& input) const {
    return { { TokenType::None, "", std::vector<Token>() }, "" };
}

ParserResult CreateFrom::parse(const std::string& input) const {
    auto currInp = input;
    Token finalToken = { TokenType::None, "", std::vector<Token>() };
    for(const auto& parser : steps) {
        const auto result = parser.parse(currInp);
        if(finalToken.type == TokenType::None) {
            finalToken = result.first;
        } else {
            finalToken = Token::pair(finalToken, result.first);
        }
        currInp = currInp.substr(result.second.length());
    }
    return { finalToken, currInp };
}
