import Text.ParserCombinators.ReadP

isConsonant :: Char -> Bool
isConsonant char = 
    all (char /=) "aeiou"

consonant :: ReadP Char
consonant =
    satisfy isConsonant

atLeastOneConsonant :: ReadP [Char]
atLeastOneConsonant =
    many1 consonant