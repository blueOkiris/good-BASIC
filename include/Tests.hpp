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
                    parser::multiple(parser::digit), input
                );
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void parseEither() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse a digit or 'a'"
                    " from ('quit' to quit): ";
                std::cin >> input;
                const auto result = parser::parse(
                    parser::either(parser::digit, parser::character('a')), input
                );
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void parseInteger() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse an integer from ('quit' to quit): ";
                std::cin >> input;
                const auto result = parser::parse(parser::integer, input);
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void parseStrings() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse strings from ('quit' to quit): ";
                std::getline(std::cin, input);
                const auto result = parser::parse(parser::str, input);
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }

        inline void parseFloats() {
            std::string input = "";
            while(input != "quit") {
                std::cout << 
                    "Enter a value to parse floats from ('quit' to quit): ";
                std::getline(std::cin, input);
                const auto result = parser::parse(parser::decimal, input);
                std::cout << "Pair: { " 
                    << result.first << ", " << result.second << " }"
                    << std::endl;
            }
        }
    }
}
