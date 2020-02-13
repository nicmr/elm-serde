module Main where

import qualified ElmP
import qualified Validator
import ElmP (ElmType(..))

-- import Data.Maybe (fromMaybe)
import Data.Either (fromRight)


ex1 = "type Foo = Bar (List String) | Baz { id : Int}"


ex2 = "type alias Dinner = { guests: Int, food: String}"

main :: IO ()
main = do
    let (parsed, rest) = last $ ElmP.parseString ex1
    putStrLn ex1
    putStrLn "Parsing ex1..."

    let generated = fromRight "fail" $ Validator.writeDecoder (ElmCustomType "Float" []) (Validator.primitiveDecoders)
    putStrLn generated

    let (parsed2, rest2) = last $ ElmP.parseString ex2
    putStrLn ex2
    putStrLn "Parsing ex2.."
    print parsed2

    let generated2 = fromRight "fail2" $ Validator.writeDecoder (parsed2) (Validator.primitiveDecoders)
    putStrLn generated2

    return ()
