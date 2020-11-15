#pragma once

#include <iostream>
#include <Parser.hpp>

namespace good_basic {
    namespace tests {
        inline void basicParseDigit() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse digits from ('quit' to quit): ";
                std::cin >> input;
                const auto result = parser::parse(parser::digit, input);
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void basicParseCharacter() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse 'a' from ('quit' to quit): ";
                std::cin >> input;
                const auto result = parser::parse(
                    parser::character('a'), input
                );
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void parseMultiDigits() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse multiple digits"
                    " from ('quit' to quit): ";
                std::cin >> input;
                const auto result = parser::parse(
                    parser::some(parser::digit), input
                );
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }
    }
}
