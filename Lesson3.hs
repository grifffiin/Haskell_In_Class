import Data.Char
letterToInteger :: Char->Int
letterToInteger x = ord (toUpper x) - ord ('A')

integerToLetter :: Int->Char
integerToLetter x = chr (x + ord('A'))

shift ::Char->Int->Char
shift c n
        | letterToInteger c + n > 25 = integerToLetter(letterToInteger c + n -26)
        | otherwise = integerToLetter (letterToInteger c + n)


encode :: [Char]->int->[Char]
encode xs n = [shift x n |x<-xs]