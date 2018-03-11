module Minilang.Parser where

import           Data.Either           (fromRight)
import           Data.Functor          (void)
import           Data.Functor.Identity (Identity)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack, unpack)
import qualified Debug.Trace
import           GHC.Generics          (Generic)
import           Prelude               hiding (pi, sum)
import           Text.Parsec
import           Text.Parsec.Char      (alphaNum)
import           Text.Parsec.Language  (haskellDef)
import qualified Text.Parsec.Token     as Tokens

data AST = U  -- universe
         | One
         | Unit
         | I Integer
         | D Double
         | Abs Binding AST
         | Ctor Text AST
         | Pi Binding AST AST
         | Sigma Binding AST AST
         | Pair AST AST
         | Sum [ Choice ]
         | Case [ Choice ]
         | Var Text
         | Ap AST AST
         | P1 AST
         | P2 AST
         | Def Decl AST
         | Err ParseError
  deriving (Eq, Show, Generic)

data Decl = Decl Binding AST AST
          | RDecl Binding AST AST
  deriving (Eq, Show, Generic)

data Binding = Pat Binding Binding
             | B Text
             | C AST
               -- ^A Constant
             | Wildcard
  deriving (Eq, Show, Generic)


data Choice = Choice Text AST
  deriving (Eq, Show, Generic)

choose :: [ Choice ] -> Text -> Maybe Choice
choose (c@(Choice t _):cs) n
  | t == n    = Just c
  | otherwise = choose cs n
choose [] _ = Nothing

-- | Top-level parser for MiniLang.
-- Reads a /MiniLang/ expression and returns its AST.
parseProgram
  :: Bool -> Text -> AST
parseProgram False =
  doParse (expr <* eof)
parseProgram True =
  debugParse (expr <* eof)

parseDecl
  :: Text -> Decl
parseDecl input =
  fromRight (Decl Wildcard Unit Unit) $ runParser (single_decl <* eof) (ParserState False) "" (unpack input)

doParse
  :: MLParser AST -> Text -> AST
doParse parser input =
  either Err id $ runParser parser (ParserState False) "" (unpack input)

debugParse
  :: MLParser AST -> Text -> AST
debugParse parser input =
  either Err id $ runParser parser (ParserState True) "" (unpack input)

-- * Parser

data ParserState = ParserState { debugParser :: Bool }
  deriving (Eq, Show)

type MLParser a = Parsec String ParserState a

expr
    :: MLParser AST
expr = debug "expr:" (
       (try def <?> "declaration")
  <|> (abstraction <?> "abstraction")
  <|> (dependent_product <?> "dependent product")
  <|> (dependent_sum  <?> "dependent sum")
  <|> (projection  <?> "projection")
  <|> (labelled_sum  <?> "labelled sum")
  <|> try case_match
  <|> try fun_type
  <|> try pair
  <|> (ctor_expr <?> "constructor")
  <|> try application
  <|> (try term  <?> "term")
  <?> "expression")

debug :: String -> MLParser a -> MLParser a
debug lbl act = do
  isDebug <- debugParser <$> getState
  if isDebug
    then getInput >>= \ s -> Debug.Trace.trace (lbl <> " :: " <> take 15 s <> (if length s > 15 then " ..." else "")) act
    else act

def
  :: MLParser AST
def = debug "definition" $ Def <$> single_decl <*> (scolon *> expr)

single_decl
  :: MLParser Decl
single_decl = debug "declaration" $ (rec_decl <|> decl)

rec_decl
  :: MLParser Decl
rec_decl = debug "recursive declaration" $ ((recur >> RDecl <$> binding <*> (colon *> expr) <*> (equal *> expr)) <?> "recursive declaration")

decl
  :: MLParser Decl
decl = debug "simple declaration" $ ((Decl <$> binding <*> (colon *> expr) <*> (equal *> expr)) <?> "declaration")

term
  :: MLParser AST
term = debug "term" ((number  <?> "number")
   <|> (try unit  <?> "unit")
   <|> (try one  <?> "one")
   <|> (try ctor  <?> "ctor")
   <|> (variable <?> "identifier")
   <|> (lpar *> expr <* rpar <?> "subexpression"))

dependent_product
  :: MLParser AST
dependent_product = debug "dependent product" $ pi >> (Pi <$> binding <*> (colon *> expr) <*> (dot *> expr))

dependent_sum
  :: MLParser AST
dependent_sum = debug "dependent sum" $ sigma >> (Sigma <$> binding <*> (colon *> expr) <*> (dot *> expr))

abstraction
  :: MLParser AST
abstraction = debug "abstraction" $ lambda >> (Abs <$> binding <*> (dot *> expr))

fun_type
  :: MLParser AST
fun_type = debug "function type" $ do
  l <- try application <|> term
  r <- (rarrow *> expr <?> "right-hand side of function type")

  pure $ Pi Wildcard l r

ctor_expr
  :: MLParser AST
ctor_expr = debug "constructor" $ do
  Ctor c _ <- ctor
  e <- option Unit expr
  pure $ Ctor c e

application
  :: MLParser AST
application = debug "application" $ ((do
  l    <- term
  r:rs <- many1 term
  pure $
    case l of
      Ctor n _ -> Ctor n r
      _        -> app l (r:rs)) <?> "application")
  where
    app l []     = l
    app l [r]    = Ap l r
    app l (r:rs) = app (Ap l r) rs

projection
  :: MLParser AST
projection = try (P1 <$> (pi1 >> dot *> expr))
         <|> P2 <$> (pi2 >> dot *> expr)

labelled_sum
  :: MLParser AST
labelled_sum = sum >> spaces >> lpar *> (Sum <$> ctors) <* rpar
  where
    ctors = sepBy clause pipe
    clause = Choice <$> identifier <*> (expr <|> pure One)

case_match
  :: MLParser AST
case_match = fun >> spaces >> lpar *> (Case <$> ctors) <* rpar
             <?> "case match"
  where
    ctors = sepBy clause pipe
    clause = Choice
           <$> identifier
           <*> (Abs
                <$> (binding <|> pure Wildcard)
                <*> (rarrow *> expr))

pair
  :: MLParser AST
pair = (lpar *> (Pair <$> expr <*> (comma *> expr)) <* rpar)
       <?> "pair"

binding
  :: MLParser Binding
binding = debug "binding" ((string "_" >> spaces *> pure Wildcard <?> "wildcard")
  <|> (B <$> identifier <?> "variable")
  <|> (C <$> number <?> "constant")
  <|> (lpar *> (Pat <$> binding <*> (comma *> binding)) <* rpar <?> "pattern")
  <?> "binding")

-- * Lexer

lexer
  :: Tokens.GenTokenParser String ParserState Identity
lexer = Tokens.makeTokenParser haskellDef

number, variable, unit, ctor, one
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer

variable = try $ do
  s <- identifier
  case s of
    "U"   -> pure U
    other -> pure $ Var other

unit   = string "()" >> spaces *> pure Unit <?> "unit"

one = string "[]" >> spaces *> pure One <?>  "One"

ctor = char '$' >> Ctor <$> identifier <*> pure Unit

identifier :: MLParser Text
identifier = debug "identifier" $ do
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

lambda, dot, colon, scolon, pi, sigma, equal, pi1, pi2, comma
  ,lpar, rpar, pipe, sum, fun, rarrow, recur, spaces1
  :: MLParser ()
lambda = char 'λ' >> spaces >> pure () <?> "lambda"
dot    = char '.' >> spaces >> pure () <?> "dot"
colon  = char ':' >> spaces >> pure () <?> "colon"
scolon = char ';' >> spaces >> pure () <?> "colon"
comma  = char ',' >> spaces >> pure () <?> "comma"
pipe   = char '|' >> spaces >> pure () <?> "pipe"
lpar   = char '(' >> spaces >> pure () <?> "left parenthesis"
rpar   = char ')' >> spaces >> pure () <?> "right parenthesis"
rarrow = (void (string "->") <|> void (char '→')) >> spaces >> pure () <?> "right arrow"
pi     = char 'Π' >> spaces >> pure ()  <?> "Pi"
pi1    = string "π1"  >> spaces >> pure () <?> "Pi.1"
pi2    = string "π2"  >> spaces >> pure () <?> "Pi.2"
sum    = string "Sum" >> spaces >> pure () <?> "Sum"
fun    = string "fun" >> spaces >> pure () <?> "fun"
recur  = string "rec" >> spaces >> pure ()  <?> "rec"
sigma  = char 'Σ' >> spaces >> pure ()  <?> "Sigma"
equal  = char '=' >> spaces >> pure ()  <?> "equal"
spaces1 = skipMany1 space <?> "spaces"
