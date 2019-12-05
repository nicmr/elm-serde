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







-- ergonomic wrapper around pipeSplit'
-- will split into variants at pipes, unless the pipe is inside a record definition
pipeSplit :: [Token] -> Maybe [[Token]]
pipeSplit ast =
    pipeSplit' ast [] [] []

-- paramters are: rest, braces, section, accumulator
pipeSplit' :: [Token] -> [Token] -> [Token] -> [[Token]] -> Maybe [[Token]]

-- pipeSplit' [] [] [] [] = [] -- should be handled by case below
-- pipeSplit' [] [] [] acc = acc -- should be handled by case below
pipeSplit' [] [] section acc = Just (section:acc)

-- context: no braces
pipeSplit' (head:rest) [] section acc =
    case head of
        Pipe -> pipeSplit' rest [] [] (section:acc) -- pipe separates variants -> section is finished
        BraceLeft -> pipeSplit' rest (BraceLeft:[]) (head:section) acc -- place new brace on stack
        BraceRight -> Nothing -- braceright with empty stack should be impossible
        t -> pipeSplit' rest [] (t:section) acc

-- context: braces (inside record type)
pipeSplit' (head:rest) (bracehead:bracerest) section acc =
    case head of
        Pipe -> pipeSplit' rest (bracehead:bracerest) (head:section) acc
        BraceLeft -> Nothing -- there is no way to nest records in records (right?)
        BraceRight -> pipeSplit' rest bracerest (head:section) acc
        t -> pipeSplit' rest [] (t:section) acc


-- semantics :: [Token] -> [Token] -> [ElmType] -> Maybe [ElmType]
-- semantics [] [] acc = Just acc
-- semantics (head:rest) [] acc =
--     case head of
--         Pipe -> semantics [] acc

-- semantics (head:rest) (bracehead:bracerest) acc =
--     case head of
--         Pipe -> if 


    



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
