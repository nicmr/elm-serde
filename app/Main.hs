module Main where

import qualified ElmP

example = "type Foo = Bar (List String) | Baz { id : Int}"

main :: IO ()
main = do
    let (parsed, rest) = last $ ElmP.parseString example
    putStrLn example
    putStrLn "Parsing Elm types..."
    print parsed
    return ()
