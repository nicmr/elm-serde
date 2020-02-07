module Validator
    where


-- module Validator
-- (
--     -- validate
-- )
-- where


import Data.Function ((&))

import ElmP (ElmType(..), ElmConstruct(..))
import qualified Data.Set
import Data.Set (Set)

import qualified Data.Map.Strict as MapS
import Data.Map.Strict (Map)

-- from: https://package.elm-lang.org/packages/elm/core/latest/
-- Default Imports

-- The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:

-- import Basics exposing (..)
-- import List exposing (List, (::))
-- import Maybe exposing (Maybe(..))
-- import Result exposing (Result(..))
-- import String exposing (String)
-- import Char exposing (Char)
-- import Tuple

-- import Debug

-- import Platform exposing ( Program )
-- import Platform.Cmd as Cmd exposing ( Cmd )
-- import Platform.Sub as Sub exposing ( Sub )



-- add Parameterized Types once support for them is implemented
-- parameterized :: Set ElmType
-- parameterized =  ["Maybe", "Result", "Tuple", "List"]

-- all basics that take to type parameters and are constructed directly from literals
primitives :: Set ElmType
primitives =
    map (\s -> ElmNewType s []) ["Int", "Float", "Bool", "String", "Char"] 
    & Data.Set.fromList

platformPrimitives :: Set ElmType
platformPrimitives =
    map (\s -> ElmNewType s []) ["Program", "Cmd", "Sub"]
    & Data.Set.fromList


data Decoder = Decoder {
    elmModule :: String,
    name :: String
}

-- qualifiedImport :: Decoder ->
-- qualifiedImport decoder existingImports =
--     , name decoder)
    

--  Map String (Set String) -> (Map String (Set String) , String)


primitiveDecoders :: Map ElmType Decoder
primitiveDecoders =
    [ ( ElmNewType "Int" [], Decoder { elmModule="Json.Decode", name="int"})
    , ( ElmNewType "Float" [], Decoder { elmModule="Json.Decode", name="float"})
    , ( ElmNewType "Bool" [], Decoder { elmModule="Json.Decode", name="bool"})
    , ( ElmNewType "String" [], Decoder { elmModule="Json.Decode", name="float"})
    , ( ElmNewType "Char" [], Decoder { elmModule="Json.Decode", name="string"}) --TODO: Decide what to do with Chars
    ]
    & MapS.fromList

writeDecoder :: ElmType -> Map ElmType Decoder -> Maybe String
writeDecoder elmtype decoders =
    case MapS.lookup elmtype decoders of
        Just (decoder) ->
            Just $ name decoder
        Nothing ->
            Nothing

writeQualifiedDecoder :: ElmType -> Map ElmType Decoder -> Maybe String
writeQualifiedDecoder elmtype decoders =
    case MapS.lookup elmtype decoders of
        Just (decoder) ->
            Just $ (elmModule decoder) ++ "." ++ (name decoder)
        Nothing ->
            Nothing
