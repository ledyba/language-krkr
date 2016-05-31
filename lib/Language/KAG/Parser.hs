{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Language.KAG.Parser (parse) where

import           Language.TJS.Parser           (withSpan)
import qualified Language.TJS.Parser           as TJS
import           Language.TJS.Tree             (SrcSpan)
import           Language.KAG.Tree             (Kag(..), KagValue(..))
import qualified Text.Parsec                   as P
import           Text.Parsec                   hiding (parse)
import           Text.Parsec.Text        (Parser)
import           Data.Text               (Text)
import           Data.String ()
import qualified Data.Text as T
import           GHC.Unicode (isSpace)

void :: Monad m => m a -> m ()
void f = f >> return ()

parse :: FilePath -> Text -> Either ParseError [Kag]
parse = P.parse source

source :: Parser [Kag]
source = do
  kagspace
  kags <- kag `sepEndBy` kagspace
  kagspace
  return kags

kag :: Parser Kag
kag = choice [
    kagNewline,
    kagTagLine,
    kagLabel,
    kagTag,
    kagText
  ]

kagText :: Parser Kag
kagText = withSpan $ do
  str <- manyTill anyChar (oneOf "*[@\n\r")
  return (KagText (T.pack str))

kagLabel :: Parser Kag
kagLabel = withSpan $ do
  void (try (string "*"))
  name <- many1 (satisfy (\t -> not(isKagSpace t || t == '\r' || t == '\n' || t == '|')))
  desc <- P.optionMaybe $ do
    void (try(string "|"))
    desc <- many1 (satisfy (\t -> not(t == '\r' || t == '\n')))
    return (T.pack desc)
  void newline
  return (KagLabel (T.pack name) desc)

kagNewline :: Parser Kag
kagNewline = withSpan $ do
  void (try newline)
  return KagNewline

kagTagLine :: Parser Kag
kagTagLine = withSpan $ do
  void (try (string "@"))
  kagspace
  r <- tagInner
  kagspace
  void P.newline
  return r

kagTag :: Parser Kag
kagTag = withSpan $ do
  void (try(string "["))
  r <- tagInner
  void (string "]")
  return r

tagInner :: Parser (SrcSpan -> Kag)
tagInner = do
  name <- identifer
  argsMaybe <- optionMaybe $ do
    kagspace1
    tagArgs `P.sepEndBy` kagspace1
  return $ case argsMaybe of
    Just args -> KagTag name args
    Nothing -> KagTag name []
  where
    tagArgs :: Parser (Text, Maybe KagValue)
    tagArgs = do
      cmd <- many1 (satisfy (\t -> not(isKagSpace t || t == '\r' || t == '\n' || t == '=' || t == ']')))
      value <- P.optionMaybe $ do
        kagspace
        try (void (string "="))
        kagspace
        str <- strLit
        return str
      return (T.pack cmd, value)

--------------------------------------------------------------------------------
-- Fixes
--------------------------------------------------------------------------------

identifer :: Parser Text
identifer = do
    f <- choice [letter, char '_']
    n <- many identChar
    return (T.pack (f:n))

identChar :: Parser Char
identChar = choice [alphaNum, char '_']

--------------------------------------------------------------------------------
-- Lit
--------------------------------------------------------------------------------


strLit :: Parser KagValue
strLit = withSpan $ do
    str <- choice [quoted, raw]
    case T.head str of
      '&'-> case P.parse TJS.expr "<<in-KAG-script>>" (T.drop 1 str) of
               Right ast -> return (KagTjsValue ast)
               Left err -> P.parserFail (show err)
      '*'-> return (KagLabelValue (T.drop 1 str))
      _  -> return (KagStrValue str)
  where
    quoted :: Parser Text
    quoted = TJS.stringLit
    raw :: Parser Text
    raw = do
      str <- many1 (satisfy (\t -> not(isKagSpace t || t == '\r' || t == '\n' || t == '=' || t == ']' || t == '\"')))
      return (T.pack str)

--------------------------------------------------------------------------------
-- WhiteSpaces
--------------------------------------------------------------------------------

isKagSpace :: Char -> Bool
isKagSpace c =  c /= '\r' && c /= '\n' && isSpace c

kagspace :: Parser ()
kagspace = skipMany comOrSpace

kagspace1 :: Parser ()
kagspace1 = skipMany1 comOrSpace

comOrSpace :: Parser ()
comOrSpace = void (satisfy isKagSpace) <|> comment
    where comment = oneline
          oneline = do
            void $ string ";"
            void $ manyTill anyChar (try $ choice [void newline, eof])
