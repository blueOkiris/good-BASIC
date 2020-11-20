using System;

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
            
            public static void Strs() {
                string input = "";
                while(input != "quit") {
                    try {
                        Console.Write("Enter strings ('quit' to quit'): ");
                        input = Console.ReadLine();
                        var result = new Str().Parse(input);
                        Console.WriteLine("Parsed: {0}", result);
                    } catch(UnexpectedTypeException ute) {
                        Console.WriteLine(ute);
                    }
                }
            }
            
            public static void Expressions() {
                string input = "";
                while(input != "quit") {
                    try {
                        Console.Write("Enter expressions ('quit' to quit'): ");
                        input = Console.ReadLine();
                        var result = new Expr().Parse(input);
                        Console.WriteLine("Parsed: {0}", result);
                    } catch(UnexpectedTypeException ute) {
                        Console.WriteLine(ute);
                    }
                }
            }
            
            public static void ReadmeExample() {
                string code = 
                    @"imports MyInterface
                    exports printHelloWorld, printWhatever, extraFunc \
                        implements MyInterface:MyInterface1
                    
                    def fn printHelloWorld() Void
                        print 'Hello, world!\n'
                    end
                    
                    def fn printWhatever(str input) Void
                        print input
                    end
                    
                    def fn extraFunc(int input)
                        return input
                    end";
                var result = new Module().Parse(code);
                Console.WriteLine("Parsed {0}", result);
            }
        }
    }
}