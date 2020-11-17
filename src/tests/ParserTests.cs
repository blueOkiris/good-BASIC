using System;
using System.Collections;

namespace GoodBasic {
    namespace Parser {
        public static class Tests {
            public static void Identifier() {
                string input = "";
                while(input != "quit") {
                    try {
                        Console.Write("Enter identifiers ('quit' to quit'): ");
                        input = Console.ReadLine();
                        var result = new Ident().Parse(input);
                        Console.WriteLine("Parsed: {0}", result);
                    } catch(UnexpectedTypeException ute) {
                        Console.WriteLine(ute);
                    }
                }
            }
            
            public static void Decimal() {
                string input = "";
                while(input != "quit") {
                    try {
                        Console.Write("Enter floats ('quit' to quit'): ");
                        input = Console.ReadLine();
                        var result = new Float().Parse(input);
                        Console.WriteLine("Parsed: {0}", result);
                    } catch(UnexpectedTypeException ute) {
                        Console.WriteLine(ute);
                    }
                }
            }
        }
    }
}