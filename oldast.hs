import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP
import qualified Data.List.Split

data Token = Pipe | Colon | TypeName [Char] | FieldName [Char] | ParLeft | ParRight | BraceLeft | BraceRight 
    deriving (Show, Eq)

parseTypeNameString :: ReadP [Char]
parseTypeNameString = do
    capitalLetter <- satisfy (\char -> char >= 'A' && char <= 'Z')
    remainingLetters <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (capitalLetter : remainingLetters)

parseTypeName :: ReadP Token
parseTypeName = do
    name <- parseTypeNameString
    return (TypeName name)

parseFieldName :: ReadP Token
parseFieldName = do
    name <- many1 (satisfy (\char -> char >= 'a' && char <= 'z'))
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

parseRHS :: ReadP [Token]
parseRHS = do
    tokens <- many1 parseToken
    return tokens


parseFieldDeclaration :: ReadP ([Char], [Char])
parseFieldDeclaration = do
    fieldname <- parseFieldName
    skipSpaces
    satisfy (\char -> char == ':')
    skipSpaces
    typename <- parseTypeNameString
    return (fieldname, typename)

parseRHSAlias :: ReadP [Token]
parseRHSAlias = do
    satisfy (\char -> char == '{')
    skipSpaces
    fields <- many parseFieldDeclaration
    

parseLHS :: ReadP [Char]
parseLHS = do
    skipSpaces
    string "type"
    skipSpaces
    name <- parseTypeNameString
    return name

parseTypeDef :: ReadP ([Char], [Token])
parseTypeDef = do
    name <- parseLHS
    skipSpaces
    satisfy (\char -> char == '=')
    rhs <- parseRHS
    eof
    return (name, rhs)

tryToElmType :: ([Char], [Token]) -> Maybe ElmType
tryToElmType (name, ast) =
    let
        variants = Data.List.Split.splitOn [Pipe] ast
    in
        Just (
            ElmType
            name
            []
            []
            [] )


data ElmBuiltIn = ElmString | ElmInt | ElmFloat
    deriving Show

data ElmType = ElmType
    { name :: [Char]
    , associated :: [ElmType]
    , variants :: [ElmType]
    , fields :: [([Char],ElmType)]
    } | ElmBuiltIn
    deriving Show

data Expr = Alternative String Expr | Parantheses Expr Expr | End



data Kinds = Simple String | Record ElmRecord

ElmRecord = {
    { name :: [Char]
    , fields :: [([Char], ElmType)] -- consider using map instead
    }
