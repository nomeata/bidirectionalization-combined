module Parser (parseProgram, parseExpression, parseFile, parseString) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Tk
import Text.ParserCombinators.Parsec.Language

import Debug.Trace 
import Data.Char (isSpace)
import Data.List (partition)

import AST


-- cnv f s = case f s of
--            Left  err -> Left $ show err
--            Right r   -> Right $ r 

parseProgram s = 
    (parse pProg "") $ insertSemi s

parseExpression = 
    (parse pExp "") 


parseString s = 
    parseProgram s 


parseFile filename =
    return . parseProgram =<< readFile filename


-- | |insertSemi| inserts ";" after every "\n".
insertSemi :: String -> String 
insertSemi []  = []
insertSemi [x] = [x]
insertSemi ('\r':'\n':x) | not (isSpace $ head x) = ';':'\r':'\n':insertSemi x 
insertSemi ('\n':x)      | not (isSpace $ head x) = ';':'\n':insertSemi x 
insertSemi ('\r':x)      | not (isSpace $ head x) = ';':'\r':insertSemi x 
insertSemi (a:x)    = a:insertSemi x


                      
varId :: Parser String
varId = do { c <- lower
           ; cs <- many (alphaNum <|> char '_')
           ; return $ (c:cs) }

conId :: Parser String
conId = do { c <- upper 
           ; cs <- many (alphaNum <|> char '_')
           ; return $ (c:cs) }

number :: Parser Int
number = do { cs <- many1 digit
            ; return $ read cs }

myLexer = Tk.makeTokenParser haskellDef 
--             $ emptyDef {
--                     commentStart = "{-"
--                   , commentEnd   = "-}"
--                   , commentLine  = "--"           
--                   , reservedNames = ["case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_" ]
--                  }



parens = Tk.parens myLexer
symbol = Tk.symbol myLexer
comma  = Tk.comma myLexer
lexeme = Tk.lexeme myLexer
reserved = Tk.reserved myLexer
brackets = Tk.brackets myLexer 
whiteSpace = Tk.whiteSpace myLexer
semi = Tk.semi myLexer


pProg = do { skipMany (whiteSpace >> semi)
           ; ds <- sepEndBy (pDecl) (many1 (whiteSpace >> semi)) -- many (lexeme pDecl)
           ; return $ assignIDsAST (AST $ ds) }


pDecl = do { whiteSpace
           ; pos <- getPosition 
           ; fName <- lexeme varId 
           ; ps    <- many1 pAPat -- parens (pPats)
           ; whiteSpace 
           ; symbol "=" 
           ; e     <- pExp
           ; return $ Decl (Name fName) FTUndet ps e }


-- pPats = sepBy pPat comma 

{-
 pPat  ::= pAPat : pPat 
        |  pCPat 
 pCPat ::= C pAPat ... pAPat 
        |  pAPat 
 pAPat ::= C | x | BList | (pPat)
 BList ::= [ pPat, ..., pPat ]
-}

pcons x y = PCon Nothing TUndet (Name "Cons") [x,y]
pnil      = PCon Nothing TUndet (Name "Nil")  []

-- list pattern 
pPat = do { whiteSpace 
          ; pos <- getPosition 
          ; try ( do { p1 <- pAPat 
                     ; symbol ":" 
                     ; p2 <- pPat 
                     ; return $ pcons p1 p2 } )
            <|> 
            pCPat }

-- constructor pattern
pCPat = do { whiteSpace
           ; pos <- getPosition 
           ; do { c <- lexeme conId
                ; ps <- many pAPat 
                ; return $ PCon Nothing TUndet (Name c) ps }
             <|> 
             pAPat }

-- pattern need not to be enclosed with parens
pAPat = do { whiteSpace 
           ; pos <- getPosition 
           ; do { c <- lexeme conId 
                ; ps <- many pAPat 
                ; return $ PCon Nothing TUndet (Name c) [] }
             <|>
             do { c <- lexeme number 
                ; return $ PCon Nothing TUndet (Name $ show c) [] }
             <|>
             do { c <- lexeme varId 
                ; return $ PVar Nothing TUndet (Name c) }
             <|>
             do { pBListPat }
             <|>
             do { parens pPat } }

-- [p1, ..., pn]                    
pBListPat = do { ps <- brackets (sepBy pPat comma)
               ; return $ foldr pcons pnil ps}

-- pPat = do { whiteSpace  
--           ; pos <- getPosition 
--           ; try pList 
--             <|> 
--             do { c <- lexeme conId                
--                ; ps <- many pAPat -- option [] $ parens pPats 
--                ; return $ PCon Nothing TUndet (Name c) ps }
--             <|>             
--             pAPat  }


-- pAPat = do { whiteSpace
--            ; pos <- getPosition 
--            ; do { c <- lexeme conId
--                 ; return $ PCon Nothing TUndet (Name c) [] }
--              <|>
--              do { c <- lexeme number 
--                 ; return $ PCon Nothing TUndet (Name $ show c) [] }
--              <|>
--              do { c <- lexeme varId 
--                 ; return $ PVar Nothing TUndet (Name c) }
--              <|>
--            --  do { pBList }
--            --  <|>
--              do { parens pPat }
--            }

-- pList = do { whiteSpace 
--            ; pos <- getPosition 
--            ; try (do { p1 <- pAPat 
--                      ; symbol ":"
--                      ; p2 <- pPat 
--                      ; return $ PCon Nothing TUndet (Name $ "Cons") [p1,p2] })
--              <|>
--              pAPat }



-- pTExp = do { whiteSpace
--            ; pos <- getPosition
--            ; do { c  <- lexeme conId
--                 ; es <- option [] $ parens (sepBy (pTExp) comma)
--                 ; return $ ECon Nothing TUndet (Name c) es }
--              <|>
--              do { c <- lexeme $ number
--                 ; return $ ECon Nothing TUndet (Name $ show c) [] }
--              <|>
--              do { c <- lexeme varId 
--                 ; do { es <- parens (sepBy (pArg) comma) 
--                      ; return $ EFun Nothing TUndet (Name c) es }
--                   <|>
--                   do { return $ EVar Nothing TUndet (Name c) } } 
--              <|> 
--              do { _ <- string "("
--                 ; c <- pTExp 
--                 ; _ <- string ")" 
--                 ; return c }}


{-
 pExp  ::= pAExp : pExp
        |  pAppExp 

 pAppExp ::= C pAExp ... pAExp
          |  f pAExp ... pAExp 
          | pAExp 

 pAPat ::= C | n | x | pBListExp | (pExp)
 pBListExp ::= [ pExp, ..., pExp ]
-}


econs x y = ECon Nothing TUndet (Name $ "Cons") [x,y]
enil      = ECon Nothing TUndet (Name $ "Nil")  [] 

-- Cons 
pExp = do { whiteSpace
          ; pos <- getPosition 
          ; try (do { e1 <- pAExp 
                    ; symbol ":" 
                    ; e2 <- pExp 
                    ; return $ econs e1 e2 })
            <|>
            pAppExp }

-- Application
pAppExp = do { whiteSpace
             ; pos <- getPosition 
             ; do { c  <- lexeme conId
                  ; es <- many pAExp -- option [] $ parens (sepBy (pExp) comma)
                  ; return $ ECon Nothing TUndet (Name c) es }
               <|>
               do { c <- lexeme varId 
                  ; do { es <- many1 pAExp --  parens (sepBy (pExp) comma) 
                       ; return $ EFun Nothing TUndet (Name c) es }
                    <|>
                    do { return $ EVar Nothing TUndet (Name c) } }                
               <|>
               pAExp }
                    
-- Atomic
pAExp = do { whiteSpace
           ; pos <- getPosition 
           ; do { c <- lexeme conId
                ; return $ ECon Nothing TUndet (Name c) [] }
             <|>
             do { c <- lexeme number 
                ; return $ ECon Nothing TUndet (Name $show c) [] }
             <|>
             do { c <- lexeme varId
                ; return $ EVar Nothing TUndet (Name c) }
             <|>
             do { pBListExp }
             <|>
             do { parens pExp }
           }

-- [e1, ..., en]                    
pBListExp = do { es <- brackets (sepBy pExp comma)
               ; return $ foldr econs enil es}


-- pExp = do { whiteSpace
--           ; pos <- getPosition
--           ; do { c  <- lexeme conId
--                ; es <- many pAExp -- option [] $ parens (sepBy (pExp) comma)
--                ; return $ ECon Nothing TUndet (Name c) es }
--             <|>
--             do { c <- lexeme $ number
--                ; return $ ECon Nothing TUndet (Name $ show c) [] }
--             <|>
--             do { c <- lexeme varId 
--                ; do { es <- many1 pAExp --  parens (sepBy (pExp) comma) 
--                     ; return $ EFun Nothing TUndet (Name c) es }
--                  <|>
--                  do { return $ EVar Nothing TUndet (Name c) } } 
--             <|> 
--             do { parens pExp }
--           }



-- pArg = do { pos <- getPosition
--           ; c <- lexeme varId
--           ; return $ EVar Nothing TUndet (Name c)} 


