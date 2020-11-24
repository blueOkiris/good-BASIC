using System;

namespace GoodBasic {
    namespace Tests {
        static partial class Tests {
            private static string exampleModule1 =
@"imports MyInterface
exports printHelloWorld, printWhatever, extraFunc \
    implements MyInterface:MyInterface1

def fn printHelloWorld() Void
    call print 'Hello, world!\n'
end

def fn printWhatever(str input) Void
    call print input
end

def fn extraFunc(int input) int
    return input
end
";
            
            public static void TestLex() {
                Console.WriteLine("Testing lexer...");
                var parser = new Parser.Parser(exampleModule1);
                var lexemes = parser.Lexemes();
                foreach(var lexeme in lexemes) {
                    Console.WriteLine(lexeme);
                }
                Console.WriteLine("Done testing lexer...");
            }
            
            public static void TestParse() {
                Console.WriteLine("Testing parser...");
                try {
                    var parser = new Parser.Parser(exampleModule1);
                    var ast = parser.Ast();
                    Console.WriteLine(ast);
                } catch(Parser.ParserException pe) {
                    Console.WriteLine(pe.Message);
                }
                Console.WriteLine("Done testing parser...");
            }
        }
    }
}
