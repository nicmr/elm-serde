import Text.ParserCombinators.ReadP

data ElmBuiltIn = ElmString | ElmInt | ElmFloat
    deriving Show

data ElmType = ElmType
    { name :: [Char]
    , associated :: [ElmType]
    , variants :: [ElmType]
    , fields :: [ElmType]
    } | ElmBuiltIn
    deriving Show


parseKeywordType :: ReadP [Char]
parseKeywordType =
    string "type"

parseTypeName :: ReadP [Char]
parseTypeName = do
    capitalLetter <- satisfy (\char -> char >= 'A' && char <= 'Z')
    remainingLetters <- many1 (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (capitalLetter : remainingLetters)

parseVariant :: ReadP [Char]
parseVariant = do
    string "|"
    skipSpaces
    typename <- parseTypeName
    return typename

    
parseVariant' :: ReadP ElmType
parseVariant' = do
    string "|"
    skipSpaces
    typename <- parseTypeName
    return (ElmType
        typename
        [] -- associated types
        [] -- variants
        []) -- fields


parseSimpleTypeDef :: ReadP [Char]
parseSimpleTypeDef = do
    string "type"
    skipSpaces
    typename <- parseTypeName
    skipSpaces
    string "="
    skipSpaces
    variants <- parseVariant
    skipSpaces
    return typename

parseSimpleTypeDef' :: ReadP ElmType
parseSimpleTypeDef' = do
    string "type"
    skipSpaces
    typename <- parseTypeName
    skipSpaces
    string "="
    skipSpaces
    variants <- many1 parseVariant'
    skipSpaces
    eof
    return (ElmType
        typename
        [] -- associated types
        variants
        []) -- fields
