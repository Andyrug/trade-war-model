{-# LANGUAGE OverloadedStrings #-}
module TradeWar.StrategyParser
    ( parseStrategy
    , Parser
    , parseTest
    , sc
    , lexeme
    , symbol
    , identifier
    , float
    , pImposeTariff
    , pRemoveTariff
    ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Data.Char (isAlphaNum)
import qualified Text.Megaparsec as M
import Text.Megaparsec (Parsec, parse, eof, ParseErrorBundle, takeWhile1P)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TradeWar.StrategyDSL
import qualified TradeWar.Types as Types

type Parser = Parsec Void Text

-- | Parse a strategy expression - just a placeholder for now
parseStrategy :: Text -> Either (ParseErrorBundle Text Void) Strategy
parseStrategy = parse (return (Primitive DoNothing)) ""

-- Re-export parseTest for convenience in the REPL
parseTest :: Show a => Parser a -> Text -> IO ()
parseTest = M.parseTest

-- | Space consumer - consumes whitespace, line and block comments
sc :: Parser ()
sc = L.space 
    space1                        -- one or more spaces
    (L.skipLineComment "--")      -- line comments
    (L.skipBlockComment "{-" "-}") -- block comments

-- | Lexeme: parse p consuming trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a specific string (token)
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse an identifier (country/industry names)
identifier :: Parser Text
identifier = lexeme $ takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '-' || c == '_' || c == ' ')

-- | Parse a floating point number
float :: Parser Double
float = lexeme L.float


-- Primitive strategy parsers
-- | Parse "impose tariff on X 0.20"
pImposeTariff :: Parser PrimitiveStrategy
pImposeTariff = do
  void $ symbol "impose"
  void $ symbol "tariff"
  void $ symbol "on"
  industry <- lexeme $ takeWhile1P (Just "industry name") isAlphaNum
  rate <- float
  return $ ImposeTariff industry rate 

pRemoveTariff :: Parser PrimitiveStrategy
pRemoveTariff = do
  void $ symbol "remove"
  void $ symbol "tariff"
  void $ symbol "on"
  industry <- lexeme $ takeWhile1P (Just "industry name") isAlphaNum
  return $ RemoveTariff industry

pNegotiate :: Parser PrimitiveStrategy
pNegotiate = do
  void $ symbol "negotiate"
  return Negotiate

pDoNothing :: Parser PrimitiveStrategy
pDoNothing = do
  void $ symbol "do"
  void $ symbol "nothing"
  return DoNothing

pMirrorLastAction :: Parser PrimitiveStrategy
pMirrorLastAction = do
  void $ symbol "mirror"
  void $ symbol "last"
  void $ symbol "action"
  return MirrorLastAction

pRetaliateAgainst :: Parser PrimitiveStrategy
pRetaliateAgainst = do
  void $ symbol "retaliate"
  void $ symbol "against"
  return RetaliateAgainst

pCooperateWith :: Parser PrimitiveStrategy
pCooperateWith = do
  void $ symbol "cooperate"
  void $ symbol "with"
  return CooperateWith



