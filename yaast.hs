import Text.ParserCombinators.ReadP

data ElmType =
    -- ElmNewtype name variants
    ElmNewType String [ElmConstruct]
    -- ElmAlias name aliasTo
    | ElmAlias String ElmConstruct
    deriving Show

parseElmType :: ReadP ElmType
parseElmType = do
    (name, alias) <- lhs
    skipSpaces
    char '='
    skipSpaces
    case alias of
        True -> do
            { rhs <- constructorRoot
            ; return (ElmAlias name rhs)
            }
        False -> do
            { rhs <- sumtypeRHS
            ; return (ElmNewType name rhs)
            }

-- parsers for the left hand side of a type declaration
lhs :: ReadP (String, Bool)
lhs = do
    skipSpaces
    string "type"
    -- not sure if this makes a difference
    -- alias <- parseAlias +++ false
    alias <- parseAlias <++ false
    skipSpaces
    name <- parseTypeName
    -- skipSpaces
    -- TODO: add support for type variables here
    return (name, alias)

false :: ReadP Bool
false = do
    return False

parseAlias :: ReadP Bool
parseAlias = do
    skipSpaces
    string "alias"
    return True


-- parsers for the rhs of a sum type declaration: several different constructors
type SumTypeRHS = [ElmConstruct]

-- parses a sumtypeRHS
-- a | b | c
sumtypeRHS :: ReadP SumTypeRHS
sumtypeRHS = do
    skipSpaces
    types <- sepBy1 constructorRoot pipe
    return types

-- parses if next non-whitespace character is a pipe
pipe = do
    skipSpaces
    char '|'


data ElmConstruct = ElmConstruct String [ElmConstruct] deriving Show
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