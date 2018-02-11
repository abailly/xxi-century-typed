module Minilang.Parser where

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text, pack, unpack)
import           GHC.Generics          (Generic)
import           Text.Parsec
import           Text.Parsec.Language  (haskellDef)
import qualified Text.Parsec.Token     as Tokens

data AST = I Integer
         | D Double
         | Var Text
         | Ap AST AST
         | Err Text
  deriving (Eq, Show, Read, Generic)

-- | Top-level parser for MiniLang.
-- Reads a /MiniLang/ expression and returns its AST.
parseML
  :: Text -> AST
parseML input =
  either (Err . pack . show) id $ runParser parser () "" (unpack input)

-- * Parser

type MLParser a = Parsec String () a

parser
  :: MLParser AST
parser = try application
     <|> term

application
  :: MLParser AST
application = Ap <$> term <*> (spaces *> parser)

term
  :: MLParser AST
term = number
   <|> identifier

-- * Lexer

lexer
  :: Tokens.GenTokenParser String () Identity
lexer = Tokens.makeTokenParser haskellDef

number, identifier
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer
identifier = Var . pack <$> Tokens.identifier lexer
