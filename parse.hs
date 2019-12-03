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

parseFirstVariant :: ReadP ElmType
parseFirstVariant = do
    typename <- parseTypeName
    return (ElmType
        typename
        [] -- associated types
        [] -- variants
        []) -- fields

parseVariant :: ReadP ElmType
parseVariant = do
    skipSpaces
    string "|"
    skipSpaces
    typename <- parseTypeName
    return (ElmType
        typename
        [] -- associated types
        [] -- variants
        []) -- fields

parseSimpleTypeDef :: ReadP ElmType
parseSimpleTypeDef = do
    string "type"
    skipSpaces
    typename <- parseTypeName
    skipSpaces
    string "="
    skipSpaces
    variant1 <- parseFirstVariant
    skipSpaces
    variants <- many parseVariant
    skipSpaces
    eof
    return (ElmType
        typename
        [] -- associated types
        (variant1 : variants)
        []) -- fields
