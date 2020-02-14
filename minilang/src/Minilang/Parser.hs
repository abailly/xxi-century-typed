module Minilang.Parser where

import           Data.Either           (fromRight)
import           Data.Functor          (void)
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack, unpack)
import qualified Debug.Trace
import           GHC.Generics          (Generic)
import           Prelude               hiding (lex, pi, sum)
import           Text.Parsec
import           Text.Parsec.Language  (haskellDef)
import qualified Text.Parsec.Token     as Tokens

data AST = U
    | One
    | Unit
    | I Integer
    | D Double
    | S String
    | Abs Binding AST
    | Ctor Text (Maybe AST)
    | Pi Binding AST AST
    | Sigma Binding AST AST
    | Pair AST AST
    | Sum [Choice]
    | Case [Clause]
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
    | Wildcard
    deriving (Eq, Show, Generic)


data Choice = Choice Text (Maybe AST)
    deriving (Eq, Show, Generic)

data Clause = Clause Text AST
    deriving (Eq, Show, Generic)

choose :: [ Choice ] -> Text -> Maybe Choice
choose (c@(Choice t _):cs) n
  | t == n    = Just c
  | otherwise = choose cs n
choose [] _ = Nothing

branch :: [ Clause ] -> Text -> Maybe Clause
branch (c@(Clause t _):cs) n
  | t == n    = Just c
  | otherwise = branch cs n
branch [] _ = Nothing

-- | Top-level parser for MiniLang.
-- Reads a /MiniLang/ expression and returns its AST.
parseProgram
  :: Bool -> Text -> AST
parseProgram False =
  doParse program
parseProgram True =
  debugParse program

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

data ParserState = ParserState
    { debugParser :: Bool
    }
    deriving (Eq, Show)

type MLParser a = Parsec String ParserState a

program
  :: MLParser AST
program = ws *> expr <* eof

expr
    :: MLParser AST
expr =
  debug "expr:" (
  (def <?> "declaration")
    <|> (abstraction <?> "abstraction")
    <|> (dependent_product <?> "dependent product")
    <|> (dependent_sum  <?> "dependent sum")
    <|> (projection  <?> "projection")
    <|> (labelled_sum  <?> "labelled sum")
    <|> (case_match <?> "case_match")
    -- try is needed because expressions are prefixes of function types
    <|> try (fun_type <?> "fun_type")
    -- try is needed because parenthesized expressions are prefixes of
    -- pairs
    <|> try (pair <?> "pair")
    <|> (ctor_expr <?> "constructor")
    <|> try application
    <|> (term  <?> "term")
    <?> "expression")

debug :: String -> MLParser a -> MLParser a
debug lbl act = do
  isDebug <- debugParser <$> getState
  if isDebug
    then getInput >>= \ s -> Debug.Trace.trace (lbl <> " :: " <> take 15 s <> (if length s > 15 then " ..." else "")) act
    else act

def
  :: MLParser AST
def = debug "definition" $ define >> Def <$> single_decl <*> fmap (fromMaybe Unit) (optionMaybe (scolon *> expr))

single_decl
  :: MLParser Decl
single_decl = debug "declaration" $ (rec_decl <|> decl)

rec_decl
  :: MLParser Decl
rec_decl = debug "recursive declaration" $ ((recur >> declaration RDecl) <?> "recursive declaration")

decl
  :: MLParser Decl
decl = debug "simple declaration" $ (declaration Decl <?> "declaration")

declaration
  :: (Binding -> AST -> AST -> b) -> MLParser b
declaration f = f <$> binding <*> (colon *> expr) <*> (equal *> expr)

term
  :: MLParser AST
term = debug "term" ((string_literal  <?> "string")
   <|> (number  <?> "number")
   <|> (unit  <?> "unit")
   <|> (one  <?> "one")
   <|> (ctor  <?> "ctor")
   <|> (variable <?> "identifier")
   <|> (lpar *> expr <* rpar <?> "subexpression"))

dependent_product
  :: MLParser AST
dependent_product = debug "dependent product" $ pi >> (uncurry Pi <$> type_ascription <*> (dot *> expr))

dependent_sum
  :: MLParser AST
dependent_sum = debug "dependent sum" $ sigma >> (uncurry Sigma <$> type_ascription <*> (dot *> expr))

abstraction
  :: MLParser AST
abstraction = debug "abstraction" $ lambda >> (Abs <$> binding <*> (dot *> expr))

type_ascription
  :: MLParser (Binding, AST)
type_ascription = (,) <$> binding <*> (colon *> expr)

fun_type
  :: MLParser AST
fun_type = debug "function type" $ do
  l <- funTypeLhs
  r <- (rarrow *> expr <?> "right-hand side of function type")

  pure $ l r
  where
    nonDependentType = Pi Wildcard <$> (try application <|> term)
    dependentType = uncurry Pi <$> (lpar *> type_ascription <* rpar)
    funTypeLhs = try dependentType <|> nonDependentType

ctor_expr
  :: MLParser AST
ctor_expr = debug "constructor" $ do
  Ctor c _ <- ctor
  e <- optionMaybe expr
  pure $ Ctor c e

application
  :: MLParser AST
application = debug "application" $ ((do
  l    <- term
  r:rs <- many1 term
  pure $ foldl Ap l (r:rs)) <?> "application")

projection
  :: MLParser AST
projection = debug "projection" $
             (P1 <$> (pi1 >> dot *> expr))
             <|> P2 <$> (pi2 >> dot *> expr)

labelled_sum
  :: MLParser AST
labelled_sum = sum >> lpar *> (Sum <$> ctors) <* rpar
  where
    ctors = clause `sepBy` pipe
    clause = Choice <$> identifier <*> optionMaybe expr

case_match
  :: MLParser AST
case_match = debug "case_match" $
  fun >> lpar *> (Case <$> ctors) <* rpar
  where
    ctors = clause `sepBy` pipe
    clause = Clause
           <$> identifier
           <*> (Abs
                <$> (binding <|> pure Wildcard)
                <*> (rarrow *> expr))

pair
  :: MLParser AST
pair = debug "pair" $ (lpar *> (Pair <$> expr <*> (comma *> expr)) <* rpar)

binding
  :: MLParser Binding
binding = debug "binding" ((string "_" >> spaces *> pure Wildcard <?> "wildcard")
  <|> try (B <$> identifier <?> "variable")
  <|> (C <$> number <?> "constant")
  <|> (lpar *> (Pat <$> binding <*> (comma *> binding)) <* rpar <?> "pattern")
  <?> "binding")

-- * Lexer

lexer
  :: Tokens.GenTokenParser String ParserState Identity
lexer = Tokens.makeTokenParser haskellDef

reservedOp :: String -> MLParser ()
reservedOp = Tokens.reservedOp lexer

lex :: MLParser a -> MLParser a
lex = Tokens.lexeme lexer

ws :: MLParser ()
ws = Tokens.whiteSpace lexer

number, string_literal, variable, unit, ctor, one
  :: MLParser AST

number = either I D <$> Tokens.naturalOrFloat lexer

string_literal = S <$> Tokens.stringLiteral lexer

variable = try $ do
  s <- identifier
  case s of
    "U"   -> pure U
    other -> pure $ Var other

unit = reservedOp "()" *> pure Unit <?> "unit"

one = reservedOp "[]" *> pure One <?>  "One"

ctor = char '$' >> Ctor <$> identifier <*> pure Nothing

identifier :: MLParser Text
identifier = debug "identifier" $ do
  i <- pack <$> lex ident
  if isReserved i
    then fail (unpack i ++ " is a keyword")
    else pure i
  where
    ident = do
      c  <- identInitial
      cs <- many identNext
      pure (c:cs)

identInitial :: MLParser Char
identInitial = letter <|> oneOf "+->*%/^?!<~#@&="

identNext :: MLParser Char
identNext = identInitial <|> digit

isReserved :: Text -> Bool
isReserved "λ"   = True
isReserved "|"   = True
isReserved "->"  = True
isReserved "="   = True
isReserved "Π"   = True
isReserved "π1"  = True
isReserved "π2"  = True
isReserved "Sum" = True
isReserved "fun" = True
isReserved "rec" = True
isReserved "def" = True
isReserved "Σ"   = True
isReserved _     = False

lambda, dot, colon, scolon, pi, sigma, equal, pi1, pi2, comma
  ,lpar, rpar, pipe, sum, fun, rarrow, define, recur, spaces1
  :: MLParser ()
lambda = lex (char 'λ') >> pure () <?> "lambda"
dot    = lex (char '.')  >> pure () <?> "dot"
colon  = lex (char ':')  >> pure () <?> "colon"
scolon = lex (char ';')  >> pure () <?> "colon"
comma  = lex (char ',')  >> pure () <?> "comma"
pipe   = lex (char '|')  >> pure () <?> "pipe"
lpar   = lex (char '(')  >> pure () <?> "left parenthesis"
rpar   = lex (char ')')  >> pure () <?> "right parenthesis"
rarrow = (void (lex (string "->")) <|> void (lex (char '→'))) >> pure () <?> "right arrow"
pi     = lex (char 'Π')  >> pure ()  <?> "Pi"
pi1    = try (string "π1"  >> spaces >> pure () <?> "Pi.1")
pi2    = try (string "π2"  >> spaces >> pure () <?> "Pi.2")
sum    = lex (string "Sum")  >> pure () <?> "Sum"
fun    = try (lex (string "fun")  >> pure () <?> "fun")
define = try (lex (string "def")  >> pure ()  <?> "def")
recur  = try (lex (string "rec")  >> pure ()  <?> "rec")
sigma  = lex (char 'Σ')  >> pure ()  <?> "Sigma"
equal  = lex (char '=')  >> pure ()  <?> "equal"
spaces1 = skipMany1 space <?> "spaces"
