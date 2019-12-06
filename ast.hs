import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP
import qualified Data.List.Split

data Token = Pipe | Colon | TypeName [Char] | FieldName [Char] | ParLeft | ParRight | BraceLeft | BraceRight 
    deriving (Show, Eq)

parseTypeNameString :: ReadP [Char]
parseTypeNameString = do
    skipSpaces
    capitalLetter <- satisfy (\char -> char >= 'A' && char <= 'Z')
    remainingLetters <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (capitalLetter : remainingLetters)

parseFieldNameString :: ReadP [Char]
parseFieldNameString = do
    first <- satisfy (\char -> char >= 'a' && char <= 'z')
    rest <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (first : rest)

parseTypeName :: ReadP Token
parseTypeName = do
    name <- parseTypeNameString
    return (TypeName name)

parseFieldName :: ReadP Token
parseFieldName = do
    name <- parseFieldNameString
    return (FieldName name)

parseParLeft :: ReadP Token
parseParLeft = do
    satisfy (\char -> char == '(')
    return ParLeft

parseParRight :: ReadP Token
parseParRight = do
    satisfy (\char -> char == ')')
    return ParRight

parsePipe :: ReadP Token
parsePipe = do
    satisfy (\char -> char == '|')
    return Pipe

parseBraceLeft :: ReadP Token
parseBraceLeft = do
    satisfy (\char -> char == '{')
    return BraceLeft

parseBraceRight :: ReadP Token
parseBraceRight = do
    satisfy (\char -> char == '}')
    return BraceRight

parseColon :: ReadP Token
parseColon = do
    satisfy (\char -> char == ':')
    return Colon
    
parseToken :: ReadP Token
parseToken = do
    skipSpaces
    token <- parseTypeName <|> parseParLeft <|> parseParRight <|> parsePipe <|>
                parseColon <|> parseBraceLeft <|> parseBraceRight
    return token

placeholder :: ReadP ElmType
placeholder = do
    return (ElmBuiltIn ElmBool)

parseRHS :: Alias -> ReadP ElmType
parseRHS alias = do
    elmtype <- case alias of
        NoAlias -> placeholder
        YesAlias -> parseSimple <|> parseAnonRecord
    return elmtype

-- no support for associativitiy precedence yet
parseSimple :: ReadP ElmType
parseSimple = do
    name <- parseTypeNameString
    associated <- many parseTypeNameString
    return ( ElmSimple name (map (\str -> ElmSimple str []) associated) )



-- no support for fields of types with associated types yet
parseFieldDeclaration :: ReadP (String, ElmType)
parseFieldDeclaration = do
    skipSpaces
    fieldname <- parseFieldNameString
    skipSpaces
    satisfy (\char -> char == ':')
    skipSpaces
    fieldtype <- parseSimple
    skipSpaces
    return (fieldname, fieldtype)

-- includes field separator character ',' for fields 1..n
fieldSeparator :: ReadP Char
fieldSeparator =
    satisfy (\char -> char == ',')

-- no support for extensible types yet
parseAnonRecord :: ReadP ElmType
parseAnonRecord = do
    satisfy (\char -> char == '{')
    skipSpaces
    fields <- sepBy1 parseFieldDeclaration fieldSeparator
    skipSpaces
    satisfy (\char -> char == '}')
    return (ElmAnonRecord fields)

parseAlias :: ReadP Alias
parseAlias = do
    string "alias"
    return YesAlias

noAlias :: ReadP Alias
noAlias = do
    return NoAlias



-- represents ast on right hand side
data Tree = Branch Tree Tree | Leaf ElmType deriving Show


leaf :: ReadP Tree
brackets p = do
    skipSpaces
    char '('
    skipSpaces
    r <- p
    skipSpaces
    char ')'
    return r

leaf :: ReadP Tree
leaf = do
    simple <- parseSimple
    return (Leaf simple)

tree :: ReadP Tree
tree = leaf +++ branch

branch :: ReadP Tree
branch = do
    a <- leaf +++ brackets tree
    skipSpaces
    char '|'
    skipSpaces
    b <- tree
    return (Branch a b)

    

parseLHS :: ReadP (String, Alias)
parseLHS = do
    skipSpaces
    string "type"
    alias <- parseAlias <|> noAlias
    skipSpaces
    name <- parseTypeNameString
    return (name, alias)


-- change to: (String, Tree), then write function to collapse Tree and build FullyQualified
parseTypeDef :: ReadP (String, [ElmType])
parseTypeDef = do
    (name, alias) <- parseLHS
    skipSpaces
    satisfy (\char -> char == '=')
    rhs <- parseRHS alias
    eof
    return (name, [rhs])

data Alias = YesAlias | NoAlias

data FullyQualified = FullyQualified String [ElmType]


data BuiltIn = ElmString | ElmInt | ElmFloat | ElmBool | ElmTuple [ElmType] | ElmList ElmType
    deriving Show

data ElmType = ElmBuiltIn BuiltIn
    -- ElmSimple name associated
    | ElmSimple String [ElmType]
    -- ElmRecord name associated fields
    | ElmRecord String [ElmType] [(String, ElmType)]
    -- ElmAnonRecord fields
    | ElmAnonRecord [(String, ElmType)]
    deriving Show

-- data ElmType = BuiltIn ElmBuiltIn
--     | Simple ElmSimple
--     | Record ElmRecord
--     | AnonRecord ElmAnonRecord
--     | SumType ElmSumType
--     deriving Show

-- data ElmBuiltIn = ElmString | ElmInt | ElmFloat | ElmBool | ElmTuple [ElmType] | ElmList ElmType
--     deriving Show
-- -- ElmSimple name associated
-- data ElmSimple = ElmSimple String [ElmSimple] deriving Show

-- -- ElmRecord name associated fields
-- data ElmRecord = ElmRecord String [ElmSimple] [(String, ElmSimple)] deriving Show

-- -- yeah this is kinda wonky
-- -- ElmAnonRecord fields
-- data ElmAnonRecord = ElmAnonRecord [(String, ElmSimple)] deriving Show

-- -- ElmSumType name associated variants
-- data ElmSumType = ElmSumType String [ElmSimple] [ElmType] deriving Show