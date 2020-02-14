module Elm.Serde.Emitter
    where

import Data.Function ((&))

import Elm.Serde

import qualified Data.Set
import Data.Set (Set)
import Data.Either
import qualified Data.Map.Strict as MapS
import Data.Map.Strict (Map)
import Data.Char (toLower)

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
    map (\s -> ElmCustomType s []) ["Int", "Float", "Bool", "String", "Char"] 
    & Data.Set.fromList

platformPrimitives :: Set ElmType
platformPrimitives =
    map (\s -> ElmCustomType s []) ["Program", "Cmd", "Sub"]
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
    [ ( ElmCustomType "Int" [], Decoder { elmModule="Json.Decode", name="int"})
    , ( ElmCustomType "Float" [], Decoder { elmModule="Json.Decode", name="float"})
    , ( ElmCustomType "Bool" [], Decoder { elmModule="Json.Decode", name="bool"})
    , ( ElmCustomType "String" [], Decoder { elmModule="Json.Decode", name="float"})
    , ( ElmCustomType "Char" [], Decoder { elmModule="Json.Decode", name="string"}) --TODO: Decide what to do with Chars
    ]
    & MapS.fromList


data EmitterError =
    Error String
    deriving (Show)


data Config = Config {
    sumTagging :: SumTaggingConfig
}

data SumTaggingConfig = Internal | External



-- more ergonomic version of writeDecoder, with primitive decoders baked in
-- to be exposed in public interface
createDecoder :: ElmType -> Config -> Either EmitterError String
createDecoder elmtype config =
    writeDecoder elmtype config primitiveDecoders


-- internal version of createDecode
-- possibly move to Internal submodue at a later point
writeDecoder :: ElmType -> Config -> Map ElmType Decoder -> Either EmitterError String
writeDecoder elmtype config decoders =
    case MapS.lookup elmtype decoders of
        Just (decoder) ->
            Right $ name decoder
        Nothing ->
            case elmtype of
                ElmCustomType name constructors ->
                    -- get signature and base of definition
                    let functionBase = (functionSignature name) ++ "\n" ++ (functionDefinition name)
                    in
                    -- then write body
                    case sumTagging config of
                        Internal ->
                            Right $ functionBase ++ fieldDecoder "type" "String"
                            -- TODO: write function that transforms string into maybe (type variant), write fail case for decoder if Nothing
                        External ->
                            Right $ functionBase ++ fieldDecoder name "Value"
                            -- TODO: value then needs to be decoded with the decoders for the different type parameters of the type
                ElmAlias name constructor ->
                    case constructor of
                        ElmConstruct name typeParams ->
                            err "Elm type aliases not yet implemented"
                        ElmRecordConstruct maybeName fields -> -- fields: [(String, TypeParam)]
                            let len = length fields
                            in
                            if len == 0 then
                                err "Zero field record not yet implemented"
                            else if len == 1 then
                                err "One field record not yet implemented"
                            else if len `elem` [2..8] then
                                let elmMap = "map" ++ (show len)
                                    fieldDecoders = foldl (\acc (name, TypeParam paramName typeParams) -> acc ++ "\n" ++ ("(field \"" ++ name ++ "\" " ++ paramName ++ ")") ) [] fields
                                in
                                    Right $ elmMap ++ " " ++ name ++ fieldDecoders
                            else err "Records with more than 8 fields not yet implemented"

    where err = Left . Error

functionSignature :: String -> String
functionSignature typeToDecode =
    (map toLower typeToDecode) ++ " : Decoder " ++ typeToDecode

functionDefinition :: String -> String
functionDefinition typeToDecode =
    (map toLower typeToDecode) ++ " = \n"

fieldDecoder :: String -> String -> String
fieldDecoder fieldName fieldType = 
    "(field \"" ++ fieldName ++ "\" " ++ fieldType ++ ")"


-- writeQualifiedDecoder :: ElmType -> Map ElmType Decoder -> Maybe String
-- writeQualifiedDecoder elmtype decoders =
--     case MapS.lookup elmtype decoders of
--         Just (decoder) ->
--             Just $ (elmModule decoder) ++ "." ++ (name decoder)
--         Nothing ->
--             Nothing
