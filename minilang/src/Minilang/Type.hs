module Minilang.Type where

import           Control.Monad       (forM_, when)
import           Control.Monad.Catch
import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import           Minilang.Eval
import           Minilang.Normalize
import           Minilang.Parser
import           Minilang.Pretty

-- * Typing

-- ** Interface

-- | Interface to type-checker environment
class (Monad tc, MonadThrow tc) =>  TypeChecker tc where

  -- | Emit an `Event` corresponding to some stage of the type-checker
  emit :: Event -> tc ()

data Event = CheckD CheckDEvent
           | CheckT CheckTEvent
           | Check  CheckEvent
           | CheckI CheckIEvent
  deriving (Eq, Show)

instance Displayable Event where
  display (CheckD e) = display e
  display (CheckT e) = display e
  display (Check  e) = display e
  display (CheckI e) = display e

data TypingError = TypingError Text
  deriving (Eq, Show)

instance Exception TypingError

typingError :: String -> TypingError
typingError = TypingError . pack

class HasLevel e where
  getLevel :: e -> Int

class Displayable e where
  display :: e -> String

-- ** Implementation

-- | Basic event output to IO
instance TypeChecker IO where
  emit _ = pure ()

-- * Typing Context

lookupType
  :: (TypeChecker tc)
  => Name -> Context -> tc Value
lookupType x (Context γ x' t)
  | x == x'               = pure t
  | otherwise             = lookupType x γ
lookupType x EmptyContext = throwM $ typingError $ "cannot find " <> show x <> " in empty context"


bindType
  :: (TypeChecker tc)
  => Binding -> Value -> Value -> Context -> tc Context
bindType (B x)     t          _ γ = pure $ Context γ x t
bindType Wildcard  _          _ γ = pure γ
bindType (Pat x y) (ESig t g) v γ = do
  γ1 <- bindType x t (p1 v) γ
  bindType y (inst g (p1 v)) (p2 v) γ1
bindType p     t v  γ             = throwM $ typingError $ "don't know how to bind " <> show (pretty p) <> " to type " <> show (pretty t) <> " and value " <> show (pretty v) <> " in context "<> show (pretty γ)

-- * Typing Judgments

-- ** Check Declaration Correctness

data CheckDEvent = CheckingDecl Decl  Int  Env  Context
                 | BoundType Binding Value Value Int Context
  deriving (Eq,Show)

instance HasLevel CheckDEvent where
  getLevel (CheckingDecl _ l _ _) = l
  getLevel (BoundType _ _ _ l _)  = l

instance Displayable CheckDEvent where
  display (CheckingDecl d _ _ _) = "checking declaration " <> show (pretty d)
  display (BoundType b v _ _ _)  = "bound " <> show (pretty b) <> " to " <> show (pretty v)


checkingDecl
  :: (TypeChecker tc)
  => Decl -> Int ->  Env -> Context -> tc ()
checkingDecl d l ρ γ = emit $ CheckD $ CheckingDecl d l ρ γ

boundType
  :: (TypeChecker tc)
  => Binding -> Value -> Value -> Int -> Context -> tc ()
boundType p t v l γ = emit $ CheckD $ BoundType p t v l γ

checkD
  :: (TypeChecker tc)
  => Int -> Decl -> Env -> Context -> tc Context

checkD l d@(Decl p a m) ρ γ = do
  checkingDecl d l ρ γ
  let
    t = eval a ρ
    v = eval m ρ
  checkT l a ρ γ
  check l m t ρ γ
  γ1 <- bindType p t v γ
  boundType p t v l γ1
  pure γ1

checkD l d@(RDecl p a m) ρ γ = do
  checkT l a ρ γ
  γ_1 <- bindType p t x_l γ
  check (l+1) m t ρ_1 γ_1
  γ' <- bindType p t v γ
  boundType p t v l γ'
  pure γ'
  where
    x_l = ENeut $ NV $ NVar l
    t   = eval a ρ
    ρ_1 = ExtendPat ρ p x_l
    v   = eval m (ExtendDecl ρ d)

-- ** Check Type Well-Formedness

data CheckTEvent = CheckingIsType AST Int Env Context
                 | CheckedIsType  AST Int Env Context
  deriving (Eq, Show)

instance HasLevel CheckTEvent where
  getLevel (CheckingIsType _ l _ _ ) = l
  getLevel (CheckedIsType _ l _ _ )  = l

instance Displayable CheckTEvent where
  display (CheckingIsType e _ _ _ ) = "checking typeness " <> show (pretty e)
  display (CheckedIsType e _ _ _ )  = "checked typeness " <> show (pretty e)

checkingIsType
  :: (TypeChecker tc)
  => AST -> Int -> Env -> Context -> tc ()
checkingIsType e l ρ γ = emit $ CheckT $ CheckingIsType e l ρ γ

checkedIsType
  :: (TypeChecker tc)
  => AST -> Int -> Env -> Context -> tc ()
checkedIsType e l ρ γ = emit $ CheckT $ CheckedIsType e l ρ γ

checkT
  :: (TypeChecker tc)
  => Int -> AST -> Env -> Context -> tc ()
checkT l e@(Sigma p a b) ρ γ = do
  checkingIsType e l ρ γ
  checkDependentT l p a b ρ γ
  checkedIsType e l ρ γ

checkT l e@(Pi p a b)    ρ γ = do
  checkingIsType e l ρ γ
  checkDependentT l p a b ρ γ
  checkedIsType e l ρ γ

checkT l U             ρ γ = checkingIsType U l ρ γ >> checkedIsType U l ρ γ
checkT l a             ρ γ = do
  checkingIsType a l ρ γ
  check l a EU ρ γ
  checkedIsType a l ρ γ

checkDependentT
  :: (TypeChecker tc)
  => Int -> Binding -> AST -> AST -> Env -> Context -> tc ()
checkDependentT l p a b ρ γ = do
  checkT l a ρ γ
  γ_1 <- bindType p (eval a ρ) x_l γ
  checkT (l+1) b ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l

-- ** Check Type assignment

data CheckEvent = CheckingHasType AST Value Int Env Context
                | CheckedHasType AST Value Int Env Context
  deriving (Eq, Show)

instance HasLevel CheckEvent where
  getLevel (CheckingHasType _ _ l _ _ ) = l
  getLevel (CheckedHasType _ _ l _ _ )  = l

instance Displayable CheckEvent where
  display (CheckingHasType e v _ ρ γ ) = "checking type of " <> show (pretty e) <> " is " <> show (pretty v) <> " in env " <> show (pretty ρ) <> " and context " <> show (pretty γ)
  display (CheckedHasType e v _ _ _ )  = "checked type of " <> show (pretty e) <> " is " <> show (pretty v)

checkingHasType
  :: (TypeChecker tc)
  => AST -> Value -> Int -> Env -> Context -> tc ()
checkingHasType a t l ρ γ = emit $ Check $ CheckingHasType a t l ρ γ

checkedHasType
  :: (TypeChecker tc)
  => AST -> Value -> Int -> Env -> Context -> tc ()
checkedHasType a t l ρ γ = emit $ Check $ CheckedHasType a t l ρ γ

check
  :: (TypeChecker tc)
  => Int -> AST -> Value -> Env -> Context -> tc ()

check l a@(Ctor c_i m) t@(ESum (c, ν)) ρ γ = do
  checkingHasType a t l ρ γ
  case choose c c_i of
    Nothing             -> throwM $ typingError ("invalid ctor " <> show c_i <> " among " <> show c <> " while typing")
    Just (Choice _ a_i) -> do
      check l m (eval a_i ν) ρ γ
      checkedHasType a t l ρ γ

check l a@(Pair m n)   ty@(ESig t g)    ρ γ = do
  checkingHasType a ty l ρ γ
  check l m t ρ γ
  check l n (inst g (eval m ρ)) ρ γ
  checkedHasType a ty l ρ γ

check l Unit     EOne  ρ  γ = checkingHasType Unit EOne l ρ γ >> checkedHasType Unit EOne l ρ γ

check l One      EU    ρ  γ = checkingHasType One EU l ρ γ >> checkedHasType One EU l ρ γ

check l a@(Case cs) ty@(EPi (ESum (cs', ν)) g) ρ γ = do
  checkingHasType a ty l ρ γ
  when (length cs /= length cs') $
    throwM $ typingError ("number of ctors in case " <> show cs <> " must be same than number in " <> show cs')
  forM_ (zip cs cs') $ \ (Choice c_i m_i, Choice c_i' a_i) -> do
    when (c_i /= c_i') $
      throwM $ typingError ("order of ctors in case must be the same same as in definition, found " <> show c_i <> ", expected " <> show c_i')
    check l m_i (EPi (eval a_i ν) (ClComp g c_i)) ρ γ
  checkedHasType a ty l ρ γ

check l e@(Sum cs) EU    ρ  γ = do
  checkingHasType e EU l ρ γ
  forM_ cs (\ (Choice _ a) -> check l a EU ρ γ)
  checkedHasType e EU l ρ γ

check l e@(Sigma p a b) EU ρ  γ = checkDependent l e p a b ρ γ

check l e@(Pi p a b) EU ρ  γ =  checkDependent l e p a b ρ γ

check l a@(Abs p m) ty@(EPi t g) ρ γ = do
  checkingHasType a ty l ρ γ
  γ_1 <- bindType p t x_l γ
  check (l+1) m (inst g x_l) ρ_1 γ_1
  checkedHasType a ty l ρ γ
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l

check l a@(Def d m) t ρ γ = do
  checkingHasType a t l ρ γ
  γ_1 <- checkD l d ρ γ
  check l m t ρ_1 γ_1
  checkedHasType a t l ρ γ
  where
    ρ_1 = ExtendDecl ρ d

check l m t ρ γ = do
  checkingHasType m t l ρ γ
  t' <- checkI l m ρ γ
  let
    norm  = normalize l t :: Normal
    norm' = normalize l t' :: Normal
  when (norm /= norm') $ throwM $
    typingError $ "[" <> show l <> "] expr " <> show m <>
    " does not have type "<> show (pretty t) <>
    ", inferred type is " <> show (pretty t') <>
    ", failed to equate normalization " <> show (pretty norm) <> " with " <> show (pretty norm') <>
    " in env " <> show (pretty ρ) <> " and context " <> show (pretty γ)
  checkedHasType m t l ρ γ


checkDependent
  :: TypeChecker m =>
     Int -> AST -> Binding -> AST -> AST -> Env -> Context -> m ()
checkDependent  l e p a b ρ γ = do
  checkingHasType e EU l ρ γ
  check l a EU  ρ γ
  γ_1 <- bindType p (eval a ρ) x_l γ
  check (l+1) b EU ρ_1 γ_1
  checkedHasType e EU l ρ γ
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l

-- ** Inter Type of an Expression

data CheckIEvent = InferringType AST Int Env Context
                 | ResolvingVariable Name Int Env Context
                 | InferredType AST Value Int Env Context
  deriving (Eq, Show)

instance HasLevel CheckIEvent where
  getLevel (InferringType _ l _ _)      = l
  getLevel (ResolvingVariable _ l _ _ ) = l
  getLevel (InferredType _ _ l _ _ )    = l

instance Displayable CheckIEvent where
  display (InferringType e _ _ _)      = "inferring type of " <> show (pretty e)
  display (ResolvingVariable n _ _ _ ) = "resolving " <> show n
  display (InferredType e v _ _ _ )    = "inferred type of " <> show (pretty e) <> " is " <> show (pretty v)

inferringType
  :: (TypeChecker tc)
  => AST -> Int -> Env -> Context -> tc ()
inferringType a l ρ γ = emit $ CheckI $ InferringType a l ρ γ

resolvingVariable
  :: (TypeChecker tc)
  => Name -> Int -> Env -> Context -> tc ()
resolvingVariable n l ρ γ = emit $ CheckI $ ResolvingVariable n l ρ γ

inferredType
  :: (TypeChecker tc)
  => AST -> Int -> Env -> Context -> Value -> tc Value
inferredType a l ρ γ v = emit (CheckI $ InferredType a v l ρ γ) >> pure v

checkI
  :: (TypeChecker tc)
  => Int -> AST -> Env -> Context -> tc Value

checkI l (Var x) ρ γ = do
  resolvingVariable x l ρ γ
  lookupType x γ >>= inferredType (Var x) l ρ γ

checkI l a@(Ap m n) ρ γ = do
  inferringType a l ρ γ
  EPi t g <- checkI l m ρ γ
  check l n t ρ γ
  let
    v = inst g (eval n ρ)
  inferredType a l ρ γ v

checkI l e ρ γ =
  throwM $ typingError $ "[" <> show l <> "] cannot infer type of " <> show (pretty e) <> " in env " <> show (pretty ρ) <> " and context " <> show (pretty γ)
