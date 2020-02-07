module ElmP
    ( parseElmType
    , ElmType (..)
    , ElmConstruct (..)
    , TypeParam (..)
    , parseString
    ) where

import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as MapStrict
import Data.Map.Strict (Map)


data ElmType =
    -- TODO: should possibly renamed to CustomType for clarification
    -- ElmNewtype name variants
    ElmNewType String [ElmConstruct]
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


parseString :: String -> [(ElmType, String)]
parseString s =
    readP_to_S parseElmType s

parseElmType :: ReadP ElmType
parseElmType =
    parseNoAlias +++ parseWithAlias

parseNoAlias :: ReadP ElmType
parseNoAlias = do
    name <- lhs
    skipSpaces
    char '='
    skipSpaces
    rhs <- sumtypeRHS
    return (ElmNewType name rhs)

parseWithAlias :: ReadP ElmType
parseWithAlias = do
    name <- lhsAlias
    skipSpaces
    char '='
    skipSpaces
    rhs <- constructorWithTypeParams +++ parseAnonRecord
    return (ElmAlias name rhs)

-- parsers for the left hand side of a type declaration
lhs :: ReadP String
lhs = do
    skipSpaces
    string "type"
    skipSpaces
    name <- parseTypeName
    return name

lhsAlias :: ReadP String
lhsAlias = do
    skipSpaces
    string "type"
    parseAlias
    skipSpaces
    name <- parseTypeName
    return name

parseAlias :: ReadP String
parseAlias = do
    skipSpaces
    string "alias"

---------------------------------------

-- parsers for the rhs of a sum type declaration: several different constructors

-- parses a sumtypeRHS
-- a | b | c
sumtypeRHS :: ReadP [ElmConstruct]
sumtypeRHS = do
    skipSpaces
    types <- sepBy1 (constructorWithTypeParams +++ parseNamedRecord) pipe
    return types

-- parses if next non-whitespace character is a pipe
pipe = do
    skipSpaces
    char '|'

---------------------------------------
    
-- parsers for the following type parameter associativity
-- A
-- ElmConstruct 'A' []

-- A B C
-- ElmConstruct 'A' [ElmConstruct 'B' [], ElmConstruct 'C' []]

-- A (B C)
-- ElmConstruct A [(ElmConstruct B [ElmConstruct C []])]

-- parses a single constructor with all its type variables or constants
-- parses a constructor that can have type parameters
constructorWithTypeParams :: ReadP ElmConstruct
constructorWithTypeParams = do
    skipSpaces
    name <- parseTypeName
    vars <- many typeParam
    return (ElmConstruct name vars)

typeParamRoot :: ReadP TypeParam
typeParamRoot = typeParamWithTypeParams +++ parantheses typeParamRoot

-- type variables can only have their own type variables if surrounded by parantheses
typeParam :: ReadP TypeParam
typeParam = typeParamNoTypeParams +++ parantheses typeParamRoot



-- parses a type parameter that can have its own type parameters
typeParamWithTypeParams :: ReadP TypeParam
typeParamWithTypeParams = do
    skipSpaces
    name <- parseTypeName
    vars <- many typeParam
    return (TypeParam name vars)


-- parses a constructor that can *not* have type parameters
typeParamNoTypeParams :: ReadP TypeParam
typeParamNoTypeParams = do
    skipSpaces
    name <- parseTypeName
    return (TypeParam name [])

-- parses parantheses
parantheses p = do
    skipSpaces
    char '('
    r <- p
    skipSpaces
    char ')'
    return r

-- parses a type/constructor name with correct capitalisation: PascalCase
parseTypeName :: ReadP [Char]
parseTypeName = do
    skipSpaces
    capitalLetter <- satisfy (\char -> char >= 'A' && char <= 'Z')
    remainingLetters <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (capitalLetter : remainingLetters)

---------------------------------------

-- parsers for elm records

parseNamedRecord :: ReadP ElmConstruct
parseNamedRecord = do
    skipSpaces
    name <- parseTypeName
    fields <- parseRecordBlock
    return (ElmRecordConstruct (Just name) fields)


parseAnonRecord :: ReadP ElmConstruct
parseAnonRecord = do
    skipSpaces
    fields <- parseRecordBlock
    return (ElmRecordConstruct Nothing fields)


-- no support for extensible types yet
-- no support for named records yet
parseRecordBlock :: ReadP [(String, TypeParam)]
parseRecordBlock = do
    skipSpaces
    satisfy (\char -> char == '{')
    skipSpaces
    fields <- sepBy1 parseFieldDeclaration fieldSeparator
    skipSpaces
    satisfy (\char -> char == '}')
    return fields
    
-- includes field separator character ',' for fields 1..n
fieldSeparator :: ReadP Char
fieldSeparator = do
    satisfy (\char -> char == ',')

-- no support for fields of types with associated types yet
parseFieldDeclaration :: ReadP (String, TypeParam)
parseFieldDeclaration = do
    skipSpaces
    fieldname <- parseFieldName
    skipSpaces
    satisfy (\char -> char == ':')
    skipSpaces
    fieldtype <- typeParamRoot
    skipSpaces
    return (fieldname, fieldtype)

parseFieldName :: ReadP String
parseFieldName = do
    first <- satisfy (\char -> char >= 'a' && char <= 'z')
    rest <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (first : rest)