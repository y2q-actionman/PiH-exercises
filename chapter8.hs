import Parsing

-- ex.1

int :: Parser Int
int = do char '-'
         natural
      +++ natural
      
-- ex.2

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             return ()
             
             
-- ex.6

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ do symbol "-"
                   e <- expr
                   return (t - e)
            +++ return t
          
-- term :: Parser Int          
-- term = do f <- factor
--           do symbol "*"
--              t <- term
--              return (f * t)
--             +++ do symbol "/"
--                    t <- term
--                    return (f `div` t)
--             +++ return f

-- factor :: Parser Int
-- factor =  do symbol "("
--              e <- expr
--              symbol ")"
--              return e
--             +++ natural

-- ex.7

term :: Parser Int          
term = do f <- expor
          do symbol "*"
             t <- term
             return (f * t)
            +++ do symbol "/"
                   t <- term
                   return (f `div` t)
            +++ return f
            
expor :: Parser Int
expor = do f <- factor
           do symbol "**"
              t <- term
              return (f ^ t)
             +++ return f

factor :: Parser Int
factor =  do symbol "("
             e <- expr
             symbol ")"
             return e
            +++ natural

-- ex.8

{-
Matigai!

-- mexpr ::= natural ('-' mexpr | _)

mexpr :: Parser Int
mexpr = do n <- natural
           do symbol "-"
              e <- mexpr
              return (n - e)
            +++ return n
-}

mexpr2 :: Parser Int
mexpr2 = do n <- natural
            es <- many (do symbol "-"
                           natural)
            return (foldl (-) n es)

