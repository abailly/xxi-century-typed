module Minilang.Parser where

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text, pack, unpack)
import           GHC.Generics          (Generic)
import           Prelude               hiding (pi)
import           Text.Parsec
import           Text.Parsec.Language  (haskellDef)
import qualified Text.Parsec.Token     as Tokens

data AST = U
         | I Integer
         | D Double
         | Var Text
         | Ap AST AST
         | Abs Binding AST
         | Pi Binding AST AST
         | Err Text
  deriving (Eq, Show, Read, Generic)

data Binding = B Text
             | Wildcard
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
parser = dependent_product
     <|> abstraction
     <|> try application
     <|> term

dependent_product
  :: MLParser AST
dependent_product = pi >> spaces >> (Pi <$> binding <*> (colon *> parser) <*> (dot *> parser))

abstraction
  :: MLParser AST
abstraction = lambda >> spaces >> (Abs <$> binding <*> (dot *> parser))


application
  :: MLParser AST
application = Ap <$> term <*> (spaces *> parser)

term
  :: MLParser AST
term = universe
   <|> number
   <|> identifier

-- * Lexer

lexer
  :: Tokens.GenTokenParser String () Identity
lexer = Tokens.makeTokenParser haskellDef

number, identifier, universe
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer
identifier = Var . pack <$> Tokens.identifier lexer
universe = string "U" *> pure U

lambda, dot, colon, pi
  :: MLParser ()
lambda = char 'λ' >> pure ()
dot = spaces >> char '.' >> spaces >> pure ()
colon = spaces >> char ':' >> spaces >> pure ()
pi = char 'Π' >> pure ()

binding
  :: MLParser Binding
binding = string "_" *> pure Wildcard
  <|> B . pack <$> Tokens.identifier lexer
