module ElmP
    ( parseElmType
    , ElmType (..)
    , ElmConstruct (..)
    , parseString
    ) where

import Text.ParserCombinators.ReadP

data ElmType =
    -- ElmNewtype name variants
    ElmNewType String [ElmConstruct]
    -- ElmAlias name aliasTo
    | ElmAlias String ElmConstruct
    deriving (Eq, Ord, Show)

data ElmConstruct =
    -- ElmConstruct name typeVars
    ElmConstruct String [ElmConstruct]
    -- ElmRecordConstruct (maybe name) [(fieldname, fieldType)]
    | ElmRecordConstruct (Maybe String) [(String, ElmConstruct)]
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
    rhs <- constructorRoot +++ parseAnonRecord
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


-- parsers for the rhs of a sum type declaration: several different constructors
type SumTypeRHS = [ElmConstruct]

-- parses a sumtypeRHS
-- a | b | c
sumtypeRHS :: ReadP SumTypeRHS
sumtypeRHS = do
    skipSpaces
    types <- sepBy1 (constructorRoot +++ parseNamedRecord) pipe
    return types

-- parses if next non-whitespace character is a pipe
pipe = do
    skipSpaces
    char '|'

    
-- parsers for the following type parameter associativity
-- A
-- ElmConstruct 'A' []

-- A B C
-- ElmConstruct 'A' [ElmConstruct 'B' [], ElmConstruct 'C' []]

-- A (B C)
-- ElmConstruct A [(ElmConstruct B [ElmConstruct C []])]

-- parses a single constructor with all its type variables or constants
constructorRoot = withTypeVars +++ parantheses constructorRoot

-- type variables can only have their own type variables if surrounded by parantheses
constructorVariable = noTypeVars +++ parantheses constructorRoot

-- parses a constructor that can have type variables or type constants
withTypeVars :: ReadP ElmConstruct
withTypeVars = do
    skipSpaces
    name <- parseTypeName
    vars <- many constructorVariable
    return (ElmConstruct name vars)

-- parses a constructor that can *not* have type variables or type constants
noTypeVars :: ReadP ElmConstruct
noTypeVars = do
    skipSpaces
    name <- parseTypeName
    return (ElmConstruct name [])

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
parseRecordBlock :: ReadP [(String, ElmConstruct)]
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
parseFieldDeclaration :: ReadP (String, ElmConstruct)
parseFieldDeclaration = do
    skipSpaces
    fieldname <- parseFieldName
    skipSpaces
    satisfy (\char -> char == ':')
    skipSpaces
    fieldtype <- constructorRoot
    skipSpaces
    return (fieldname, fieldtype)

parseFieldName :: ReadP String
parseFieldName = do
    first <- satisfy (\char -> char >= 'a' && char <= 'z')
    rest <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (first : rest)