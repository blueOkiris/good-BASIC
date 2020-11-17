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

ParserResult SelectFrom::parse(const std::string& input) const {
    return { { TokenType::None, "", std::vector<Token>() }, "" };
}

ParserResult CreateFrom::parse(const std::string& input) const {
    return { { TokenType::None, "", std::vector<Token>() }, "" };
}
