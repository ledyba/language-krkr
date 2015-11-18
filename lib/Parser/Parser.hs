module Parser.Parser (parse,parse2) where

import           Data.Char                     (chr, digitToInt)
import           Numeric                       (readHex, readInt, readOct)
import           Parser.Tree                   (Stmt(..)
                                               ,Expr (..)
                                               ,Identifer (..)
                                               ,ApplyArg(..))
import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

import Control.Monad (liftM)

void :: Monad m => m a -> m ()
void f = f >> return ()

parse :: FilePath -> String -> Either ParseError Stmt
parse = P.parse source

parse2 :: FilePath -> String -> Either ParseError Stmt
parse2 = P.parse switchStmt

source :: Parser Stmt
source = do
    tjspace
    src <- stmt
    tjspace
    eof
    return src

--expr = choice [numLit, strLit, identifer]
stmt :: Parser Stmt
stmt = choice
          [try ifStmt
          ,try switchStmt
          ,try whileStmt
          ,try tryStmt
          ,execStmt]

--------------------------------------------------------------------------------
-- Stmt
--------------------------------------------------------------------------------
ifStmt :: Parser Stmt
ifStmt = do
    void (string "if" >> tjspace >> char '(' >> tjspace)
    econd <- expr
    void (tjspace >> char ')' >> tjspace)
    strue <- stmt
    sfalse <- optionMaybe elseStmt
    return (If econd strue sfalse)
  where
    elseStmt = do
      tjspace
      void (string "else")
      tjspace
      stmt

breakStmt ::  Parser Stmt
breakStmt = string "break" >> tjspace >> char ';' >> return Break

switchStmt :: Parser Stmt
switchStmt = do
    void (string "switch" >> tjspace >> char '(')
    econd <- expr
    void (tjspace >> char ')' >> tjspace >> char '{' >> tjspace)
    cases <- try switchCase `sepEndBy` tjspace
    def <- optionMaybe (try switchDefault)
    void (tjspace >> char '}')
    return (Switch econd cases def)
  where
    switchCase :: Parser (Expr, [Stmt])
    switchCase = do
      try $ void (string "case" >> tjspace)
      caseCond <- expr
      void (tjspace >> char ':' >> tjspace)
      caseStmt <- try (choice [try breakStmt, try stmt]) `sepEndBy` tjspace
      return (caseCond, caseStmt)
    switchDefault :: Parser [Stmt]
    switchDefault = do
      try $ void (string "default" >> tjspace >> char ':' >> tjspace)
      try (choice [try breakStmt, try stmt]) `sepEndBy` tjspace

whileStmt :: Parser Stmt
whileStmt = do
  void (string "while")
  tjspace
  void (char '(')
  tjspace
  econd <- expr
  tjspace
  void (char ')')
  tjspace
  dostmt <- stmt
  return (While econd dostmt)

execStmt :: Parser Stmt
execStmt = do
  e <- expr
  tjspace
  void (char ';')
  return (Exec e)

tryStmt :: Parser Stmt
tryStmt = do
    void (string "try" >> tjspace)
    stmt0 <- stmt
    void (tjspace >> string "catch" >> tjspace >> char '(' >> tjspace)
    name <- identifer
    void (tjspace >> char ')' >> tjspace)
    stmt1 <- stmt
    return (Try stmt0 name stmt1)

--------------------------------------------------------------------------------
-- Expr + Term
--------------------------------------------------------------------------------

expr' :: [String] -> Parser Expr -> Parser Expr
expr' ops bottom =
    do
      e <- bottom
      es <- many (try rep)
      tjspace
      return (foldl fld e es)
  where
    fld e (op,e1) = Bin op e e1
    rep :: Parser (String, Expr)
    rep = do
      tjspace
      op <- try $ choice (fmap (try.string) ops)
      tjspace
      e <- try bottom
      return (op, e)
--

expr :: Parser Expr
expr = expr' [","] expr15

expr15 :: Parser Expr
expr15 = expr' ["instanceof"] expr14

expr14 :: Parser Expr
expr14 = expr' ["incontextof"] expr13

expr13 :: Parser Expr
expr13 = expr' ["=","<->","&=","|=","^=","-=","+=","%=","/=","\\=","*=","||=","&&=",">>=","<<=",">>>="] expr12

expr12 :: Parser Expr
expr12 =
  do
    c <- expr11
    try (do
        tjspace
        void $ char '?'
        tjspace
        e1 <- expr12
        tjspace
        void $ char ':'
        tjspace
        e2 <- expr12
        return (Tri c e1 e2)
      ) <|> return c

expr11 :: Parser Expr
expr11 = expr' ["||"] expr10

expr10 :: Parser Expr
expr10 = expr' ["&&"] expr9

expr9 :: Parser Expr
expr9 = expr' ["|"] expr8

expr8 :: Parser Expr
expr8 = expr' ["^"] expr7

expr7 :: Parser Expr
expr7 = expr' ["&"] expr6

expr6 :: Parser Expr
expr6 = expr' ["===","!==","==","!="] expr5

expr5 :: Parser Expr
expr5 = expr' [">=","<=",">","<"] expr4

expr4 :: Parser Expr
expr4 = expr' [">>","<<",">>>"] expr3

expr3 :: Parser Expr
expr3 = expr' ["+","-"] expr2

expr2 :: Parser Expr
expr2 = expr' ["%","/","\\","*"] expr1

expr1 :: Parser Expr
expr1 = do
    mcast <- optionMaybe (do
      void $ char '('
      tjspace
      name <- identifer
      tjspace
      void $ char ')'
      return (Cast name))
    case mcast of
      Just cast -> liftM cast expr1
      Nothing -> preOp

preOp :: Parser Expr
preOp = do
          ops <- choice (fmap (try.string) ["!","~","--","++","new","invalidate","delete", "typeof", "#", "$", "+", "-", "&", "*"]) `sepEndBy` tjspace
          e <- postOp
          return (foldl (flip PreUni) e (reverse ops))

postOp :: Parser Expr
postOp = do
    e0 <- term
    tjspace
    as <- apply `sepEndBy` tjspace
    return (foldl (\e a -> a e) e0 as)
  where
    apply :: Parser (Expr -> Expr)
    apply = try (choice [
          do
            op <- choice (fmap (try.string) ["++", "--", "!", "isvalid"])
            return (`PostUni` op)
          ,do
            void $ char '['
            tjspace
            e1 <- expr
            tjspace
            void $ char ']'
            return (`Index` e1)
          ,do
            void $ char '('
            tjspace
            exprs <- applyArg `sepBy` (tjspace >> char ',' >> tjspace)
            tjspace
            void $ char ')'
            return (`Call` exprs)
          ,do
            void $ char '.'
            tjspace
            name <- identifer
            return (`Dot` name)
        ])
        where
          applyArg = try leftApply <|> exprApply
          leftApply = (void (char '*') <|> void(string "...")) >> return ApplyLeft
          exprApply = do
            e <- expr
            try (void (char '*') >> return (ApplyArray e)) <|>  return (ApplyExpr e)

term :: Parser Expr
term = choice
    [numLit
    ,strLit
    ,Dot WithThis <$> (char '.' >> tjspace >> identifer)
    ,dictLit
    ,arrayLit
    ,do
      void $ char '('
      tjspace
      e <- expr
      tjspace
      void $ char ')'
      return e
    ,fmap Ident identifer
    ]

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

identifer :: Parser Identifer
identifer = do
    f <- choice [letter, char '_']
    n <- many $ choice [alphaNum, char '_']
    return $ Identifer (f:n)

arrayLit :: Parser Expr
arrayLit = do
    void $ char '['
    tjspace
    lits <- expr `sepEndBy` char ','
    tjspace
    void $ char ']'
    return $ Array lits

dictLit :: Parser Expr
dictLit = do
    void $ string "%["
    tjspace
    lits <- dictItem `sepEndBy` char ','
    tjspace
    void $ char ']'
    return $ Dict lits
  where
    dictItem :: Parser (String, Expr)
    dictItem = do
      key <- stringLit
      tjspace
      void $ string "=>"
      tjspace
      value <- expr
      return (key, value)

uniLit :: Parser Char
uniLit = do
    str <- many1 hexDigit
    case readHex str of
      [] -> fail ("Could not read as hex: 0x" ++ str)
      (s,_) : _ -> return $ chr s

zeroLit :: Parser Expr
zeroLit = char '0' >> return (Int 0)

octLit :: Parser Expr
octLit = do
    void $ char '0'
    n <- many1 octDigit
    case readOct n of
      [] -> fail ("Could not read as oct: " ++ n)
      (s,_) : _ -> return $ Int s

hexLit :: Parser Expr
hexLit = do
    void $ char '0'
    r <- oneOf "xX"
    n <- many1 hexDigit
    case readHex n of
      [] -> fail ("Could not read as hex: 0" ++ r:n)
      (s,_) : _ -> return $ Int s

binLit :: Parser Expr
binLit = do
  void $ char '0'
  r <- oneOf "bB"
  n <- many1 (oneOf "01")
  case readInt 2 (`elem` "01") digitToInt n of
    [] -> fail ("Could not read as binary: 0" ++ r : n)
    (s,_) : _ -> return (Int s)

decLit :: Parser Expr
decLit = do
    d <- decIntLit
    f <- optionMaybe decFloatLit
    e <- optionMaybe decExpLit
    return $ case (f,e) of
      (Nothing, Nothing) -> Int $ fromInteger d
      (Just f1, Nothing) -> Real $ fromInteger d + f1
      (Nothing, Just e1) -> Int $ fromInteger (d * (10 ^ e1))
      (Just f1, Just e1) -> Real $ (fromInteger d + f1) * fromInteger (10 ^ e1)
    where
      decIntLit :: Parser Integer
      decIntLit = do
          f <- oneOf "123456789"
          n <- many digit
          return (read (f : n))

      decFloatLit :: Parser Double
      decFloatLit = do
          void $ char '.'
          ds <- many1 digit
          return $ read $ '0':'.':ds

      decExpLit :: Parser Integer
      decExpLit = do
          void $ oneOf "eE"
          s <- option 1 (char '-' >> return (-1))
          ds <- many1 digit
          return $ s * read ds

numLit :: Parser Expr
numLit = choice [try decLit, try octLit, try hexLit, try binLit, zeroLit]

strLit :: Parser Expr
strLit = Str <$> stringLit

stringLit :: Parser String
stringLit = choice [singleStringLit, doubleStringLit]
  where
    singleStringLitEsc :: Parser Char
    singleStringLitEsc = char '\\' >> choice
                [ char '\\' >> return '\\',
                 char '\'' >> return '\'',
                 char 'a' >> return (chr 0x7),
                 char 'b' >> return (chr 0x8),
                 char 'f' >> return (chr 0xc),
                 char 'n' >> return '\n',
                 char 'r' >> return '\r',
                 char 't' >> return '\t',
                 char 'v' >> return (chr 0xb),
                 oneOf "xX" >> uniLit
                ]
    singleStringLit :: Parser String
    singleStringLit = do
        void $ char '\''
        str <- many $ choice [singleStringLitEsc, P.noneOf "\'"]
        void $ char '\''
        return str
    doubleStringLitEsc :: Parser Char
    doubleStringLitEsc = char '\\' >> choice
                [ char '\\' >> return '\\',
                 char '\"' >> return '"',
                 char 'a' >> return (chr 0x7),
                 char 'b' >> return (chr 0x8),
                 char 'f' >> return (chr 0xc),
                 char 'n' >> return '\n',
                 char 'r' >> return '\r',
                 char 't' >> return '\t',
                 char 'v' >> return (chr 0xb),
                 oneOf "xX" >> uniLit
                ]
    doubleStringLit :: Parser String
    doubleStringLit = do
        void $ char '\"'
        str <- many $ choice [doubleStringLitEsc, P.noneOf "\""]
        void $ char '\"'
        return str

--------------------------------------------------------------------------------
-- WhiteSpaces
--------------------------------------------------------------------------------

tjspace :: Parser ()
tjspace = skipMany (void space <|> comment)
    where comment = choice [try oneline, try multline]
          oneline = do
            void $ string "//"
            void $ manyTill anyChar (try $ choice [void newline, eof])
          multline = do
            void $ string "/*"
            void $ manyTill anyChar (try (string "*/"))
