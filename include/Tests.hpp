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
                for(const auto& item : result) {
                    std::cout << "Pair: { " 
                        << item.first << ", " << item.second << " }"
                        << std::endl;
                }
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
                for(const auto& item : result) {
                    std::cout << "Pair: { " 
                        << item.first << ", " << item.second << " }"
                        << std::endl;
                }
            }
        }
    }
}
