module Parser (parseProgram, parseExpression, parseFile, parseString) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Tk
import Text.ParserCombinators.Parsec.Language

import AST

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

myLexer = Tk.makeTokenParser 
            $ emptyDef {
                    commentStart = "{-"
                  , commentEnd   = "-}"
                  , commentLine  = "--"           
                  , reservedNames = ["let", "in","case","data","type"]
                 }

parens = Tk.parens myLexer
symbol = Tk.symbol myLexer
comma  = Tk.comma myLexer
lexeme = Tk.lexeme myLexer
reserved = Tk.reserved myLexer
whiteSpace = Tk.whiteSpace myLexer


cnv f s = case f s of
            Left  err -> Left $ show err
            Right r   -> Right $ r 

parseProgram = 
    (parse pProg "")

parseExpression = 
    (parse pExp "")


parseString s = 
    parseProgram s 


parseFile filename =
    return . parseProgram =<< readFile filename


pProg = do { whiteSpace
           ; ds <- many (lexeme pDecl)
           ; return $ assignIDsAST (AST ds) }


pDecl = do { pos <- getPosition 
            ; fName <- lexeme varId 
            ; ps    <- parens (pPats)
            ; symbol "=" 
            ; e     <- pExp
            ; return $ Decl (Name fName) FTUndet ps e }


pPats = sepBy pPat comma 


pPat = do { pos <- getPosition 
          ; do { c <- lexeme conId                
               ; ps <- option [] $ parens pPats 
               ; return $ PCon Nothing TUndet (Name c) ps }
            <|> 
            do { c <- lexeme $ number
               ; return $ PCon Nothing TUndet (Name $show c) [] }
            <|>
            do { c <- lexeme varId
               ; return $ PVar Nothing TUndet (Name c) }
            <|>
            do { _ <- string "("
               ; p <- pPat 
               ; _ <- string ")" 
               ; return p } }


pTExp = do { whiteSpace
           ; pos <- getPosition
           ; do { c  <- lexeme conId
                ; es <- option [] $ parens (sepBy (pTExp) comma)
                ; return $ ECon Nothing TUndet (Name c) es }
             <|>
             do { c <- lexeme $ number
                ; return $ ECon Nothing TUndet (Name $ show c) [] }
             <|>
             do { c <- lexeme varId 
                ; do { es <- parens (sepBy (pArg) comma) 
                     ; return $ EFun Nothing TUndet (Name c) es }
                  <|>
                  do { return $ EVar Nothing TUndet (Name c) } } 
             <|> 
             do { _ <- string "("
                ; c <- pTExp 
                ; _ <- string ")" 
                ; return c }}

pExp = do { whiteSpace
          ; pos <- getPosition
          ; do { c  <- lexeme conId
               ; es <- option [] $ parens (sepBy (pExp) comma)
               ; return $ ECon Nothing TUndet (Name c) es }
            <|>
            do { c <- lexeme $ number
               ; return $ ECon Nothing TUndet (Name $ show c) [] }
            <|>
            do { c <- lexeme varId 
               ; do { es <- parens (sepBy (pExp) comma) 
                    ; return $ EFun Nothing TUndet (Name c) es }
                 <|>
                 do { return $ EVar Nothing TUndet (Name c) } } 
            <|> 
            do { _ <- string "("
               ; e <- pExp
               ; _ <- string ")"
               ; return e }
          }


pArg = do { pos <- getPosition
          ; c <- lexeme varId
          ; return $ EVar Nothing TUndet (Name c)} 


