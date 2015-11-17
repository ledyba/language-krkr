module Parser.Parser (parse) where

import           Control.Applicative           ((<$>))
import           Data.Char                     (chr, digitToInt)
import           Numeric                       (readHex, readInt, readOct)
import           Parser.Tree                   (Expr (..), Identifer (..),
                                                Tree (..))
import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

void :: Monad m => m a -> m ()
void f = f >> return ()

parse :: String -> Either ParseError Tree
parse = P.parse source "<TEXT>"

source :: Parser Tree
source = do
    tjspace
    src <- Expr <$> expr
    tjspace
    eof
    return src

expr :: Parser Expr
expr = choice [numLit, strLit, identifer]

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

term :: Parser Expr
term = do
  tjspace
  r <- choice
    [numLit
    ,strLit
    ,Bin "." WithThis <$> (char '.' >> tjspace >> identifer)
    ,arrayLit
    ,dictLit
    ,do
      void $ char '('
      e <- expr
      void $ char ')'
      return e
    ]
  tjspace
  return r

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

identifer :: Parser Expr
identifer = do
    f <- choice [letter, char '_']
    n <- many $ choice [alphaNum, char '_']
    return $ Ident $ Identifer (f:n)

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

decIntLit :: Parser Integer
decIntLit = do
    f <- oneOf "123456789"
    n <- many digit
    return (read (f : n))

octLit :: Parser Expr
octLit = do
    void $ char '0'
    n <- many octDigit
    case readOct n of
      [] -> fail ("Could not read as oct: " ++ n)
      (s,_) : _ -> return $ Int s

hexLit :: Parser Expr
hexLit = do
    void $ char '0'
    r <- oneOf "xX"
    n <- many hexDigit
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
numLit = choice [decLit, octLit, hexLit, binLit, zeroLit]

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
    where comment = choice [oneline, multline]
          oneline = do
            void $ try $ string "//"
            void $ manyTill anyChar (try $ choice [void newline, eof])
          multline = do
            void $ try $ string "/*"
            void $ manyTill anyChar (try (string "*/"))
