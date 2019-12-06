import Text.ParserCombinators.ReadP


-- parsers for top level sum type alternatives
type SumType = [ElmType]

-- parses a sumtype
-- a | b | c
sumtype :: ReadP SumType
sumtype = do
    skipSpaces
    types <- sepBy1 mytree pipe
    return types

-- parses if next non-whitespace character is a pipe
pipe = do
    skipSpaces
    char '|'

-- parsers for the following type parameter associativity
-- A
-- ElmType 'A' []

-- A B C
-- ElmType 'A' [ElmType 'B' [], ElmType 'C']

-- A (B C)
-- ElmType A [(ElmType B [ElmType C []])]
data ElmType = ElmType String [ElmType] deriving Show

assoctree = myassociated +++ brackets mytype

mytree = mytype +++ brackets mytype

-- parses a type that can have type parameters
mytype :: ReadP ElmType
mytype = do
    skipSpaces
    name <- parseTypeName
    assoc <- many assoctree
    return (ElmType name assoc)

-- parses a type that can not have type parameters
myassociated :: ReadP ElmType
myassociated = do
    skipSpaces
    name <- parseTypeName
    return (ElmType name [])

-- parses brackets
brackets p = do
    skipSpaces
    char '('
    r <- p
    skipSpaces
    char ')'
    return r

-- parses a type name with correct capitalisation: PascalCase
parseTypeName :: ReadP [Char]
parseTypeName = do
    skipSpaces
    capitalLetter <- satisfy (\char -> char >= 'A' && char <= 'Z')
    remainingLetters <- many (satisfy (\char -> char >= 'A' && char <= 'z'))
    return (capitalLetter : remainingLetters)