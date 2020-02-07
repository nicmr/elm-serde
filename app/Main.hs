module Main where

import qualified ElmP
import qualified Validator
import ElmP (ElmType(..))

import Data.Maybe (fromMaybe)


ex1 = "type Foo = Bar (List String) | Baz { id : Int}"


ex2 = "type alias Dinner = { guests: Int, food: String}"

main :: IO ()
main = do
    let (parsed, rest) = last $ ElmP.parseString ex1
    putStrLn ex1
    putStrLn "Parsing ex1..."

    let (parsed2, rest2) = last $ ElmP.parseString ex2
    putStrLn ex2
    putStrLn "Parsing ex2.."
    print parsed2

    let generated = fromMaybe "fail" $ Validator.writeDecoder (ElmNewType "Float" []) (Validator.primitiveDecoders)
    putStrLn generated


    let generated2 = fromMaybe "fail2" $ Validator.writeDecoder (parsed2) (Validator.primitiveDecoders)
    putStrLn generated2

    return ()
