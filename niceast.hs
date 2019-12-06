import Text.ParserCombinators.ReadP




-- data Tree = Branch Tree Tree | Leaf String deriving Show
   
data Operator = And | Or deriving Show
   
data OrTree = Branch Operator Tree Tree | Leaf ElmType deriving Show

-- type name associated
data ElmType = ElmType String [String] deriving Show


brackets p = do
    char '('
    r <- p
    char ')'
    return r


associated = do
    skipSpaces
    name <- (many1 (choice (map char ['a' .. 'z'])))
    return name

leaf = do
    name <- many1 (choice (map char ['a'..'z']))
    skipSpaces
    associated <- many associated ++ 
    return (Leaf (ElmType name associated))

andBranch = do
    a <- leaf +++ brackets tree
    char '&'
    b <- andTree
    return (Branch And a b)

andTree = leaf +++ brackets tree +++ andBranch

orBranch = do
    a <- andTree +++ brackets tree
    char '|'
    b <- orTree
    return (Branch Or a b)

orTree = andTree +++ brackets tree +++ orBranch

tree = orTree