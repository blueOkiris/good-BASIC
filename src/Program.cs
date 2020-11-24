using System;

namespace GoodBasic {
    class Program {
        public static void Main(string[] args) {
            Tests.Tests.TestLex();
            //Tests.Tests.TestParseModuleHeader();
            Tests.Tests.TestSimpleParse();
        }
    }
}
