module Main where

import qualified ElmP

example = "type Foo = Bar | Baz { id : Int}"

main :: IO ()
main = do
    -- (parsed, rest) <- last . ElmP.parseString example
    putStrLn example
    putStrLn "Parsing Elm types..."
    print example
