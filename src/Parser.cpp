#include <vector>
#include <exception>
#include <string>
#include <Parser.hpp>

using namespace good_basic;

ParserResult SelectFrom::parse(const std::string& input) const {
    return { Token(), "" };
}

ParserResult CreateFrom::parse(const std::string& input) const {
    return { Token(), "" };
}
