#pragma once

#include <iostream>
#include <memory>
#include <Parser.hpp>

namespace good_basic {
    namespace tests {
        inline void manyAs() {
            std::string input = "";
            while(input != "quit") {
                std::cout
                    << "Enter something to parse 'a's from ('quit' to quit):";
                getline(std::cin, input);
                const auto parser = Many(std::make_shared<Char>(Char('a')));
                try {
                    const auto result = parser.parse(input);
                    std::cout
                        << "Result: { " << result.first.str() << ", "
                        << result.second << " }" << std::endl;
                } catch(const UnexpectedTokenException& ute) {
                    std::cout << ute.what() << std::endl;
                }
            }
        }
    }
}
