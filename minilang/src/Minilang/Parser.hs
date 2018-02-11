module Minilang.Parser where

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text, pack, unpack)
import           GHC.Generics          (Generic)
import           Prelude               hiding (pi, sum)
import           Text.Parsec
import           Text.Parsec.Language  (haskellDef)
import qualified Text.Parsec.Token     as Tokens

data AST = U | Unit
         | I Integer
         | D Double
         | Var Text
         | Ap AST AST
         | Abs Binding AST
         | Pi Binding AST AST
         | Sigma Binding AST AST
         | P1 AST | P2 AST
         | Pair AST AST
         | Decl Binding AST AST
         | Sum [ Ctor ]
         | Case [ Choice ]
         | Err Text
  deriving (Eq, Show, Read, Generic)

data Binding = B Text
             | Wildcard
  deriving (Eq, Show, Read, Generic)

data Ctor = Ctor Text AST
  deriving (Eq, Show, Read, Generic)

data Choice = Choice Text AST AST
  deriving (Eq, Show, Read, Generic)

-- | Top-level parser for MiniLang.
-- Reads a /MiniLang/ expression and returns its AST.
parseML
  :: Text -> AST
parseML =
  doParse program

parseMLExpr
  :: Text -> AST
parseMLExpr =
  doParse (expr <* eof)

doParse
  :: MLParser AST -> Text -> AST
doParse parser input =
  either (Err . pack . show) id $ runParser parser () "" (unpack input)

-- * Parser

type MLParser a = Parsec String () a

program
  :: MLParser AST
program = decl

decl
  :: MLParser AST
decl = Decl <$> binding <*> (colon *> expr) <*> (equal *> expr)

expr
    :: MLParser AST
expr = dependent_product
   <|> dependent_sum
   <|> abstraction
   <|> projection
   <|> labelled_sum
   <|> try case_match
   <|> try pair
   <|> try application
   <|> term

term
  :: MLParser AST
term = number
   <|> unit
   <|> identifier

dependent_product
  :: MLParser AST
dependent_product = pi >> spaces >> (Pi <$> binding <*> (colon *> expr) <*> (dot *> expr))

dependent_sum
  :: MLParser AST
dependent_sum = sigma >> spaces >> (Sigma <$> binding <*> (colon *> expr) <*> (dot *> expr))

abstraction
  :: MLParser AST
abstraction = lambda >> spaces >> (Abs <$> binding <*> (dot *> expr))

projection
  :: MLParser AST
projection = try (P1 <$> (pi1 >> dot *> expr))
         <|> P2 <$> (pi2 >> dot *> expr)

labelled_sum
  :: MLParser AST
labelled_sum = sum >> spaces >> lpar *> (Sum <$> ctors) <* rpar
  where
    ctors = sepBy ctor pipe
    ctor = Ctor <$> (pack <$> Tokens.identifier lexer) <*> (spaces *> expr <|> pure Unit)

case_match
  :: MLParser AST
case_match = fun >> spaces >> lpar *> (Case <$> ctors) <* rpar
  where
    ctors = sepBy ctor pipe
    ctor = Choice
           <$> (pack <$> Tokens.identifier lexer)
           <*> (spaces *> expr <|> pure Unit)
           <*> (rarrow *> expr)

pair
  :: MLParser AST
pair = lpar *> (Pair <$> expr <*> (comma *> expr)) <* rpar

application
  :: MLParser AST
application = Ap <$> term <*> (spaces *> expr)

-- * Lexer

lexer
  :: Tokens.GenTokenParser String () Identity
lexer = Tokens.makeTokenParser haskellDef

number, identifier, unit
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer
identifier = do
  s <- Tokens.identifier lexer
  case s of
    "U"   -> pure U
    other -> pure $ Var (pack other)
unit   = string "()" *> pure Unit

lambda, dot, colon, pi, sigma, equal, pi1, pi2, comma
  ,lpar, rpar, pipe, sum, fun, rarrow
  :: MLParser ()
lambda = char 'λ' >> pure ()
dot    = spaces >> char '.' >> spaces >> pure ()
colon  = spaces >> char ':' >> spaces >> pure ()
comma  = spaces >> char ',' >> spaces >> pure ()
pipe   = spaces >> char '|' >> spaces >> pure ()
lpar   = char '(' >> spaces >> pure ()
rpar   = spaces >> char ')' >> pure ()
rarrow = spaces >> string "->" >> spaces >> pure ()
pi     = char 'Π' >> pure ()
pi1    = string "π1" >> pure ()
pi2    = string "π2" >> pure ()
sum    = string "Sum" >> pure ()
fun    = string "fun" >> pure ()
sigma  = char 'Σ' >> pure ()
equal  = char '=' >> spaces >> pure ()

binding
  :: MLParser Binding
binding = string "_" *> pure Wildcard
  <|> B . pack <$> Tokens.identifier lexer
