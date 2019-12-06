import Text.ParserCombinators.ReadP


-- parsers for the rhs of a sum type declaration : the alternative constructors
type SumTypeRHS = [ElmConstruct]

-- parses a sumtype
-- a | b | c
sumtype :: ReadP SumTypeRHS
sumtype = do
    skipSpaces
    types <- sepBy1 constructorRoot pipe
    return types

-- parses if next non-whitespace character is a pipe
pipe = do
    skipSpaces
    char '|'

-- parsers for the following type parameter associativity
-- A
-- ElmConstruct 'A' []

-- A B C
-- ElmConstruct 'A' [ElmConstruct 'B' [], ElmConstruct 'C']

-- A (B C)
-- ElmConstruct A [(ElmConstruct B [ElmConstruct C []])]
data ElmConstruct = ElmConstruct String [ElmConstruct] deriving Show

constructorRoot = withTypeVars +++ parantheses constructorRoot

-- constructor type variables can only have their own type variables if surrounded by parantheses
constructorVariable = noTypeVars +++ parantheses constructorRoot

-- parses a constructor that can have type variables / type constants
withTypeVars :: ReadP ElmConstruct
withTypeVars = do
    skipSpaces
    name <- parseTypeName
    vars <- many constructorVariable
    return (ElmConstruct name vars)

-- parses a constructor that can *not* have type variables / type constants
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