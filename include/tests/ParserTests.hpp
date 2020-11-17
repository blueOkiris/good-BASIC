#pragma once

#include <iostream>
#include <memory>
#include <parser/Parser.hpp>

namespace good_basic {
    namespace parser {
        namespace tests {
            inline void manyAs() {
                std::string input = "";
                while(input != "quit") {
                    std::cout
                        << "Enter something to parse 'a's from "
                            "('quit' to quit):";
                    getline(std::cin, input);
                    const auto parser = Many(ptr(Char('a')));
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
            
            inline void digits() {
                std::string input = "";
                while(input != "quit") {
                    std::cout
                        << "Enter something to parse digits from "
                            "('quit' to quit):";
                    getline(std::cin, input);
                    const auto parser = Many(ptr(Digit()));
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
}
