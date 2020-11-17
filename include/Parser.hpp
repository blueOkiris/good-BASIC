#pragma once

#include <vector>
#include <string>
#include <exception>

namespace good_basic {
    struct Token {
        
    };
    
    typedef std::pair<Token, std::string> ParserResult;
    struct Parser {
        virtual ParserResult parse(const std::string& input) const = 0;
    };

    struct SelectFrom : public Parser {
        const std::vector<Parser> options;
        ParserResult parse(const std::string& input) const override;
    };

    struct CreateFrom : public Parser {
        const std::vector<Parser> steps;
        ParserResult parse(const std::string& input) const override;
    };
}
