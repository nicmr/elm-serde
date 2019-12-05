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

parseRHS :: ReadP [Token]
parseRHS = do
    tokens <- many1 parseToken
    return tokens

-- no support for associativitiy precedence yet
parseSimple :: ReadP ElmSimple
parseSimple = do
    name <- parseTypeNameString
    associated <- many parseTypeNameString
    return ( ElmSimple name (map (\str -> ElmSimple str []) associated) )



-- no support for fields of types with associated types yet
parseFieldDeclaration :: ReadP (String, ElmSimple)
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
parseAnonRecord :: ReadP ElmAnonRecord
parseAnonRecord = do
    satisfy (\char -> char == '{')
    skipSpaces
    fields <- sepBy1 parseFieldDeclaration fieldSeparator
    skipSpaces
    satisfy (\char -> char == '}')
    return (ElmAnonRecord fields)
    

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

data ElmType = BuiltIn ElmBuiltIn
    | Simple ElmSimple
    | Record ElmRecord
    | AnonRecord ElmAnonRecord
    | SumType ElmSumType
    deriving Show

-- ElmSimple name associated
data ElmSimple = ElmSimple String [ElmSimple] deriving Show

-- ElmRecord name associated fields
data ElmRecord = ElmRecord String [ElmSimple] [(String, ElmSimple)] deriving Show

-- yeah this is kinda wonky
-- ElmAnonRecord fields
data ElmAnonRecord = ElmAnonRecord [(String, ElmSimple)] deriving Show

-- ElmSumType name associated variants
data ElmSumType = ElmSumType String [ElmSimple] [ElmType] deriving Show

data ElmBuiltIn = ElmString | ElmInt | ElmFloat | ElmBool | ElmTuple [ElmType] | ElmList ElmType
    deriving Show
    





-- recursive data structure approach
-- data Expr = JustType ElmType | WithAssociated Type [Type] | Alternative Expr Expr | Parantheses Expr Expr | End
-- data Kinds = Simple String | Record ElmRecord





-- weird approach 


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