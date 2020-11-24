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
                var parser = new Parser.Parser(exampleModule1);
                var lexemes = parser.Lexemes();
                
                foreach(var lexeme in lexemes) {
                    Console.WriteLine(lexeme);
                }
            }
        }
    }
}
