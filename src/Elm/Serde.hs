module Elm.Serde where

data ElmType =
    -- TODO: should possibly renamed to CustomType for clarification
    -- ElmCustomType name variants
    ElmCustomType String [ElmConstruct]
    -- ElmAlias name aliasTo
    | ElmAlias String ElmConstruct
    deriving (Eq, Ord, Show)

data ElmConstruct =
    -- ElmConstruct name typeVars
    ElmConstruct String [TypeParam]
    -- ElmRecordConstruct (maybe name) [(fieldname, fieldType)]
    | ElmRecordConstruct (Maybe String) [(String, TypeParam)]
    deriving (Eq, Ord, Show)

-- A TypeParam can represent either
--    - a type variable: `a` in `(List a)`
--    - a type constant: `String` in `(List String)`
-- the parser can't know which kind it is though, so creating a sum type at this point doesn't make sense
-- Which kind of type param it is will have to be determined by validating the AST later
data TypeParam = TypeParam String [TypeParam]
    deriving (Eq, Ord, Show)