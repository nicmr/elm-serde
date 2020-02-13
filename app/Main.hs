module Main where

import qualified Elm.Serde.Emitter as Emitter
import qualified Elm.Serde.Parser as Parser
import Elm.Serde.Parser (ElmType(..))

-- import Data.Maybe (fromMaybe)
import Data.Either (fromRight)


ex1 = "type Foo = Bar (List String) | Baz { id : Int}"


ex2 = "type alias Dinner = { guests: Int, food: String}"

main :: IO ()
main = do
    let (parsed, rest) = last $ Parser.parseString ex1
    putStrLn ex1
    putStrLn "Parsing ex1..."

    let generated = fromRight "fail" $ Emitter.writeDecoder (ElmCustomType "Float" []) (Emitter.primitiveDecoders)
    putStrLn generated

    let (parsed2, rest2) = last $ Parser.parseString ex2
    putStrLn ex2
    putStrLn "Parsing ex2.."
    print parsed2

    let generated2 = fromRight "fail2" $ Emitter.writeDecoder (parsed2) (Emitter.primitiveDecoders)
    putStrLn generated2

    return ()
