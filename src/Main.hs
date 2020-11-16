module Main(main) where

import System.IO(hSetBuffering, stdout, BufferMode(..))
import Tests

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    testStrings
