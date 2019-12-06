import Text.ParserCombinators.ReadP

data MyType = MyType String [MyType] deriving Show

mytype :: ReadP MyType
mytype = do
    skipSpaces
    name <- char 'o'
    assoc <- many assoctree
    return (MyType [name] assoc)

myassociated :: ReadP MyType
myassociated = do
    skipSpaces
    name <- char 'o'
    return (MyType [name] [])

brackets p = do
    skipSpaces
    char '('
    r <- p
    skipSpaces
    char ')'
    return r

assoctree = myassociated +++ brackets mytype
mytree = mytype +++ brackets mytype

-- 
-- a b c
-- MyType a (cons b cons c cons nil)
-- collapse:
-- MyType a [b, c]

-- a (b c)
-- MyType a (cons ( b cons c cons nil ) cons nil)
-- collapse:
-- MyType a [(MyType b [c])]

-- a
-- MyType a cons nil
-- MyType a []