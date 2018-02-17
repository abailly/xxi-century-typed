module Minilang.Parser where

import           Data.Functor          (void)
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
         | RDecl Binding AST AST
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
  doParse (program <* eof)

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
program = rec_decl
      <|> decl

rec_decl
  :: MLParser AST
rec_decl = recur >> RDecl <$> binding <*> (colon *> expr) <*> (equal *> expr)

decl
  :: MLParser AST
decl = Decl <$> binding <*> (colon *> expr) <*> (equal *> expr)

expr
    :: MLParser AST
expr = dependent_product
   <|> dependent_sum
   <|> projection
   <|> labelled_sum
   <|> abstraction
   <|> try case_match
   <|> try fun_type
   <|> try pair
   <|> try application
   <|> term

term
  :: MLParser AST
term = number
   <|> unit
   <|> identifier
   <|> lpar *> expr <* rpar

dependent_product
  :: MLParser AST
dependent_product = pi >> spaces >> (Pi <$> binding <*> (colon *> expr) <*> (dot *> expr))

dependent_sum
  :: MLParser AST
dependent_sum = sigma >> spaces >> (Sigma <$> binding <*> (colon *> expr) <*> (dot *> expr))

abstraction
  :: MLParser AST
abstraction = lambda >> spaces >> (Abs <$> binding <*> (dot *> expr))

fun_type
  :: MLParser AST
fun_type = do
  l <- term
  r <- rarrow *> expr
  pure $ Pi Wildcard l r


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
           <*> (spaces *> term <|> pure Unit)
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

number, variable, unit
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer
variable = try $ do
  s <- identifier
  case s of
    "U"   -> pure U
    other -> pure $ Var other
unit   = string "()" *> pure Unit <?> "unit"

identifier :: MLParser Text
identifier = do
  i <- pack <$> ident <* spaces
  if isReserved i
    then fail (unpack i ++ " is a keyword")
    else pure i
  where
    ident = do
      c  <- letter
      cs <- many alphaNum
      pure (c:cs)

isReserved :: Text -> Bool
isReserved "λ"   = True
isReserved "Π"   = True
isReserved "π1"  = True
isReserved "π2"  = True
isReserved "Sum" = True
isReserved "fun" = True
isReserved "rec" = True
isReserved "Σ"   = True
isReserved _     = False

lambda, dot, colon, pi, sigma, equal, pi1, pi2, comma
  ,lpar, rpar, pipe, sum, fun, rarrow, recur, spaces1
  :: MLParser ()
lambda = char 'λ' >> spaces >> pure () <?> "lambda"
dot    = char '.' >> spaces >> pure () <?> "dot"
colon  = char ':' >> spaces >> pure () <?> "colon"
comma  = char ',' >> spaces >> pure () <?> "comma"
pipe   = char '|' >> spaces >> pure () <?> "pipe"
lpar   = char '(' >> spaces >> pure () <?> "left parenthesis"
rpar   = char ')' >> spaces >> pure () <?> "right parenthesis"
rarrow = (void (string "->") <|> void (char '→')) >> spaces >>
         (getInput >>= \ s -> trace s $ pure ()) <?> "right arrow"
pi     = char 'Π' >> spaces >> pure ()  <?> "Pi"
pi1    = string "π1"  >> spaces >> pure () <?> "Pi.1"
pi2    = string "π2"  >> spaces >> pure () <?> "Pi.2"
sum    = string "Sum" >> spaces >> pure () <?> "Sum"
fun    = string "fun" >> spaces >> pure () <?> "fun"
recur  = string "rec" >> spaces >> pure ()  <?> "rec"
sigma  = char 'Σ' >> spaces >> pure ()  <?> "Sigma"
equal  = char '=' >> spaces >> pure ()  <?> "equal"
spaces1 = skipMany1 space <?> "spaces"
