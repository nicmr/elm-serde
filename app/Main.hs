module Main where

import qualified Elm.Serde.Emitter as Emitter
import Elm.Serde.Emitter (sumTagging, SumTaggingConfig(..))


import qualified Elm.Serde.Parser as Parser
import Elm.Serde.Parser (ElmType(..))

-- import Data.Maybe (fromMaybe)
import Data.Either (fromRight)

main :: IO ()
main = do

    exampleSrc <- readFile "examples/Example.elm"
    putStrLn "file contents:"
    putStrLn exampleSrc
    let (parsedTypes, _) = last $ Parser.parseString exampleSrc
    putStrLn "Parsing Elm types..."
    print parsedTypes
    putStrLn "Generating decoders..."
    let decoders = map (\t -> Emitter.createDecoder t (Emitter.Config{ sumTagging = External}) ) parsedTypes
    print decoders

    return ()
