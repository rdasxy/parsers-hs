newtype Parser a = Parser (String -> [(a,String)])
parse (Parser p) = p

instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f = Parser (\cs -> concat [parse (f a) cs' |
                              (a,cs') <- parse p cs])
                              
item :: Parser Char
item = Parser (\cs -> case cs of
              "" -> []
              (c:cs) -> [(c,cs)])

p :: Parser (Char,Char)
p = do c <- item
       item
       d <- item
       return (c,d)
