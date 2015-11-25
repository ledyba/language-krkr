module Language.TJS.Parser (parse,parse2) where

import           Data.Char                     (chr, digitToInt)
import           Numeric                       (readHex, readInt, readOct)
import           Language.TJS.Tree             (Stmt(..)
                                               ,Expr (..)
                                               ,Identifer (..)
                                               ,ApplyArg(..)
                                               ,FuncArg(..)
                                               ,SrcSpan(..))
import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

withSpan :: Parser (SrcSpan -> a) -> Parser a
withSpan m = do
  from <- getPosition
  c <- m
  to <- getPosition
  return (c (SrcSpan from to))

void :: Monad m => m a -> m ()
void f = f >> return ()

parse :: FilePath -> String -> Either ParseError Stmt
parse = P.parse source

parse2 :: FilePath -> String -> Either ParseError Stmt
parse2 = P.parse classStmt

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
          [ifStmt
          ,switchStmt
          ,whileStmt
          ,withStmt
          ,tryStmt
          ,classStmt
          ,functionStmt
          ,forStmt
          ,throwStmt
          ,returnStmt
          ,blockStmt
          ,breakStmt
          ,continueStmt
          ,propStmt
          ,varStmt
          ,execStmt]

--------------------------------------------------------------------------------
-- Stmt
--------------------------------------------------------------------------------
ifStmt :: Parser Stmt
ifStmt = withSpan $ do
    void (try (string "if"))
    tjspace >> char '(' >> tjspace
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
breakStmt = withSpan $ try (string "break" >> tjspace >> char ';') >> return Break

continueStmt ::  Parser Stmt
continueStmt = withSpan $ try (string "continue" >> tjspace >> char ';') >> return Continue

switchStmt :: Parser Stmt
switchStmt = withSpan $ do
    void (try (string "switch" >> tjspace >> char '('))
    tjspace
    econd <- expr
    void (tjspace >> char ')' >> tjspace >> char '{' >> tjspace)
    cases <- try switchCase `sepEndBy` tjspace
    def <- optionMaybe (try switchDefault)
    void (tjspace >> char '}')
    return (Switch econd cases def)
  where
    switchCase :: Parser (Expr, [Stmt])
    switchCase = do
      void (try (string "case") >> tjspace1)
      caseCond <- expr
      void (tjspace >> char ':' >> tjspace)
      caseStmt <- try (choice [try breakStmt, try stmt]) `sepEndBy` tjspace
      return (caseCond, caseStmt)
    switchDefault :: Parser [Stmt]
    switchDefault = do
      void (try (string "default" >> tjspace >> char ':'))
      tjspace
      stmt `sepEndBy` tjspace

whileStmt :: Parser Stmt
whileStmt = withSpan $ do
  void (try (string "while" >> tjspace >> char '('))
  tjspace
  econd <- expr
  tjspace >> char ')' >> tjspace
  dostmt <- stmt
  return (While econd dostmt)

withStmt :: Parser Stmt
withStmt = withSpan $ do
  void (try (string "with" >> tjspace >> void (char '(')))
  tjspace
  econd <- expr
  void (tjspace >> char ')' >> tjspace)
  innerStmts <- stmt
  return (With econd innerStmts)

blockStmt :: Parser Stmt
blockStmt = withSpan $ do
  try (char '{') >> tjspace
  stmts <- try stmt `sepEndBy` tjspace
  void (char '}')
  return (Block stmts)

execStmt :: Parser Stmt
execStmt = withSpan $ do
  e <- expr
  void (tjspace >> char ';')
  return (Exec e)

varStmt :: Parser Stmt
varStmt = withSpan $ do
    void (try (string "var" >> tjspace1))
    binds <- bind `sepBy` (tjspace >> char ',' >> tjspace)
    void (tjspace >> char ';')
    return (Var binds)
  where
    bind = do
      name <- identifer
      value <- optionMaybe ( try tjspace >> char '=' >> tjspace >> expr15 )
      return (name,value)

tryStmt :: Parser Stmt
tryStmt = withSpan $ do
    void (try (string "try" >> notFollowedBy identChar))
    tjspace
    stmt0 <- stmt
    void (tjspace >> string "catch" >> tjspace >> char '(' >> tjspace)
    name <- identifer
    void (tjspace >> char ')' >> tjspace)
    stmt1 <- stmt
    return (Try stmt0 name stmt1)

forStmt :: Parser Stmt
forStmt = withSpan $ do
    void (try (string "for" >> tjspace >> char '(' ))
    tjspace
    e0 <- expr
    void (tjspace >> char ';' >> tjspace)
    e1 <- expr
    void (tjspace >> char ';' >> tjspace)
    e2 <- expr
    void (tjspace >> char ')' >> tjspace)
    stmt0 <- stmt
    return (For e0 e1 e2 stmt0)

functionStmt :: Parser Stmt
functionStmt = withSpan $ do
    void (try (string "function" >> notFollowedBy identChar))
    name <- optionMaybe (try tjspace1 >> identifer)
    tjspace
    args <- option [] argsList
    tjspace
    f <- blockStmt
    return (Func name args f)
  where
    argsList = do
      try (char '(') >> tjspace
      args <- arg `sepBy` (tjspace >> char ',' >> tjspace)
      void (tjspace >> char ')')
      return args
    arg = choice [star, withName]
    star = try (char '*') >> return FuncLeft
    withName = do
      n <- identifer
      tjspace
      a <- (try (char '*') >> return FuncArray) <|> defArg
      return (a n)
    defArg = do
      tjspace
      v <- optionMaybe (char '=' >> tjspace >> expr15)
      return (`FuncArg` v)

throwStmt :: Parser Stmt
throwStmt = keywordStmt' "throw" Throw

returnStmt :: Parser Stmt
returnStmt = keywordStmt' "return" Return

keywordStmt' :: String -> (Expr -> SrcSpan -> Stmt) -> Parser Stmt
keywordStmt' keyword cstr = withSpan $ do
  try (string keyword >> tjspace1)
  e <- expr
  void (tjspace >> char ';')
  return (cstr e)

classStmt :: Parser Stmt
classStmt = withSpan $ do
  try (string "class" >> tjspace1)
  name <- identifer
  tjspace >> char '{' >> tjspace
  stmts <- (varStmt <|> functionStmt <|> propStmt) `sepEndBy` tjspace
  void $ tjspace >> char '}'
  return (Class name stmts)

propStmt :: Parser Stmt
propStmt = withSpan $ do
    try (string "property" >> tjspace1)
    name <- identifer
    tjspace >> char '{' >> tjspace
    (getter,setter) <- one <|> two
    void $ tjspace >> char '}'
    return (Prop name getter setter)
  where
    one = do
      getter <- getterStmt
      tjspace
      setter <- optionMaybe setterStmt
      return (Just getter,setter)
    two = do
      setter <- setterStmt
      tjspace
      getter <- optionMaybe getterStmt
      return (getter, Just setter)
    getterStmt = do
      try (void (string "getter"))
      tjspace
      optional (char '(' >> tjspace >> char ')')
      tjspace
      blockStmt
    setterStmt = do
      try (void (string "setter"))
      tjspace >> char '(' >> tjspace
      arg <- identifer
      tjspace >> char ')' >> tjspace
      content <- blockStmt
      return (arg,content)


--------------------------------------------------------------------------------
-- Expr + Term
--------------------------------------------------------------------------------

expr' :: [String] -> Parser Expr -> Parser Expr
expr' ops bottom =
    do
      from <- getPosition
      e <- bottom
      es <- many (try rep)
      tjspace
      return (foldl (fld from) e es)
  where
    fld from e (op,e1,to) = Bin op e e1 (SrcSpan from to)
    rep :: Parser (String, Expr, SourcePos)
    rep = do
      tjspace
      op <- try $ choice (fmap (try.string) ops)
      tjspace
      e <- try bottom
      to <- getPosition
      return (op, e, to)
--

expr :: Parser Expr
expr = expr' ["if"] expr16

expr16 :: Parser Expr
expr16 = expr' [","] expr15

expr15 :: Parser Expr
expr15 = expr' ["instanceof"] expr14

expr14 :: Parser Expr
expr14 = expr' ["incontextof"] expr13

expr13 :: Parser Expr
expr13 = expr' ["=","<->","&=","|=","^=","-=","+=","%=","/=","\\=","*=","||=","&&=",">>=","<<=",">>>="] expr12

expr12 :: Parser Expr
expr12 =
  do
    from <- getPosition
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
        to <- getPosition
        return (Tri c e1 e2 (SrcSpan from to))
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
    from <- getPosition
    mcast <- optionMaybe (do
      void $ char '('
      tjspace
      name <- identifer
      tjspace
      void $ char ')'
      return (Cast name))
    case mcast of
      Just cast ->
        do
          e <- expr1
          to <- getPosition
          return (cast e (SrcSpan from to))
      Nothing -> preOp

preOp :: Parser Expr
preOp = do
          ops <- choice (fmap p ["!","~","--","++","new","invalidate","delete", "typeof", "#", "$", "+", "-", "&", "*"]) `sepEndBy` tjspace
          e <- postOp
          to <- getPosition
          return (foldl (\e0 (from, op) -> PreUni op e0 (SrcSpan from to)) e (reverse ops))
        where
          p s = do
            from <- getPosition
            void (try (string s))
            return (from,s)

postOp :: Parser Expr
postOp = do
    from <- getPosition
    e0 <- term
    tjspace
    as <- apply `sepEndBy` tjspace
    return (foldl (\e (a,to) -> a e (SrcSpan from to)) e0 as)
  where
    apply :: Parser (Expr -> SrcSpan -> Expr, SourcePos)
    apply = choice [
           do
            op <- try (choice (fmap (try.string) ["++", "--", "!", "isvalid"]))
            to <- getPosition
            return ((`PostUni` op), to)
          ,do
            void $ try (char '[')
            tjspace
            e1 <- expr
            tjspace
            void $ char ']'
            to <- getPosition
            return ((`Index` e1), to)
          ,do
            void $ try (char '(')
            tjspace
            exprs <- applyArg `sepBy` (tjspace >> char ',' >> tjspace)
            tjspace
            void $ char ')'
            to <- getPosition
            return ((`Call` exprs), to)
          ,do
            void $ try (char '.')
            tjspace
            name <- identifer
            to <- getPosition
            return ((`Dot` name), to)
        ]
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
    ,withSpan $ Dot <$> withSpan (char '.' >> return WithThis) <*> (tjspace >> identifer)
    ,dictLit
    ,arrayLit
    ,do
      void $ char '('
      tjspace
      e <- expr
      tjspace
      void $ char ')'
      return e
    ,withSpan $ fmap Ident identifer
    ]

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

identifer :: Parser Identifer
identifer = do
    f <- choice [letter, char '_']
    n <- many identChar
    return $ Identifer (f:n)

identChar :: Parser Char
identChar = choice [alphaNum, char '_']

arrayLit :: Parser Expr
arrayLit = withSpan $ do
    void $ char '['
    tjspace
    lits <- expr `sepEndBy` char ','
    tjspace
    void $ char ']'
    return $ Array lits

dictLit :: Parser Expr
dictLit = withSpan $ do
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
zeroLit = withSpan $ char '0' >> return (Int 0)

octLit :: Parser Expr
octLit = withSpan $ do
    void $ char '0'
    n <- many1 octDigit
    case readOct n of
      [] -> fail ("Could not read as oct: " ++ n)
      (s,_) : _ -> return $ Int s

hexLit :: Parser Expr
hexLit = withSpan $ do
    void $ char '0'
    r <- oneOf "xX"
    n <- many1 hexDigit
    case readHex n of
      [] -> fail ("Could not read as hex: 0" ++ r:n)
      (s,_) : _ -> return $ Int s

binLit :: Parser Expr
binLit = withSpan $ do
  void $ char '0'
  r <- oneOf "bB"
  n <- many1 (oneOf "01")
  case readInt 2 (`elem` "01") digitToInt n of
    [] -> fail ("Could not read as binary: 0" ++ r : n)
    (s,_) : _ -> return (Int s)

decLit :: Parser Expr
decLit = withSpan $ do
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
strLit = withSpan $ Str <$> stringLit

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
tjspace = skipMany comOrSpace

tjspace1 :: Parser ()
tjspace1 = skipMany1 comOrSpace

comOrSpace :: Parser ()
comOrSpace = void space <|> comment
    where comment = choice [try oneline, try multline]
          oneline = do
            void $ string "//"
            void $ manyTill anyChar (try $ choice [void newline, eof])
          multline = do
            void $ string "/*"
            void $ manyTill anyChar (try (string "*/"))
