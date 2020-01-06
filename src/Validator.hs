module Validator
    (
        -- validate
    )
    where

import Data.Function ((&))

import ElmP (ElmType(..), ElmConstruct(..))
import qualified Data.Set
import Data.Set (Set)


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



-- elmBasics :: Set ElmType
-- elmBasics =  ["Maybe", "Result", "Tuple", "Cmd", "Sub", "Program"]

-- all basics that take to type parameters and are constructed directly from literals
elmPrimitives :: Set ElmType
elmPrimitives =
    map (\s -> ElmNewType s []) ["Int", "Float", "Bool", "String", "Char"] 
    & Data.Set.fromList



-- validate :: Set ElmType -> [ElmType] -> Some [ElmType]
-- validate basics elmTypes =


