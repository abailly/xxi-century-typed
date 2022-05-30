{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Minilang.Type where

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Catch (Exception, MonadThrow (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Minilang.Env (Env' (..), Name)
import Minilang.Eval (
    Context,
    Context' (Context, EmptyContext),
    Env,
    FunClos (ClComp, ClComp0),
    NVar (NVar),
    Neutral (NV),
    SumClos (SumClos),
    Value (..),
    eval,
    inst,
    p1,
    p2,
 )
import Minilang.Normalize (Normal, Normalize (normalize), same)
import Minilang.Parser (
    AST (..),
    Binding (B, Pat, Wildcard),
    Choice (Choice),
    Clause (Clause),
    Decl (..),
    choose,
 )
import Minilang.Pretty (pretty)
import Minilang.Primitives (
    PrimType (PrimDouble, PrimInt, PrimString),
 )

-- * Typing

-- ** Interface

-- | Interface to type-checker environment
class (Monad tc, MonadThrow tc) => TypeChecker tc where
    -- | Emit an `Event` corresponding to some stage of the type-checker
    emit :: Event -> tc ()

data Event
    = CheckD CheckDEvent
    | CheckT CheckTEvent
    | Check CheckEvent
    | CheckI CheckIEvent
    deriving (Eq, Show)

instance Displayable Event where
    display (CheckD e) = display e
    display (CheckT e) = display e
    display (Check e) = display e
    display (CheckI e) = display e

newtype TypingError = TypingError Text
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

lookupType ::
    (TypeChecker tc) =>
    Name ->
    Context ->
    tc Value
lookupType x (Context γ x' t)
    | x == x' = pure t
    | otherwise = lookupType x γ
lookupType "Int" EmptyContext = pure $ EU 0
lookupType "Double" EmptyContext = pure $ EU 0
lookupType "String" EmptyContext = pure $ EU 0
lookupType x EmptyContext = throwM $ typingError $ "cannot find " <> show x <> " in empty context"

bindType ::
    (TypeChecker tc) =>
    Binding ->
    Value ->
    Value ->
    Context ->
    tc Context
bindType (B x) t _ γ = pure $ Context γ x t
bindType Wildcard _ _ γ = pure γ
bindType (Pat x y) (ESig t g) v γ = do
    γ1 <- bindType x t (p1 v) γ
    bindType y (inst g (p1 v)) (p2 v) γ1
bindType p t v γ =
    throwM $
        typingError $
            "don't know how to bind "
                <> show (pretty p)
                <> " to type "
                <> show (pretty t)
                <> " and value "
                <> show (pretty v)
                <> " in context "
                <> show (pretty γ)
-- * Typing Judgments

-- ** Check Declaration Correctness

data CheckDEvent
    = CheckingDecl Decl Int Env Context
    | BoundType Binding Value Value Int Context
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasLevel CheckDEvent where
    getLevel (CheckingDecl _ l _ _) = l
    getLevel (BoundType _ _ _ l _) = l

instance Displayable CheckDEvent where
    display (CheckingDecl d _ _ _) = "checking declaration " <> show (pretty d)
    display (BoundType b v _ _ _) = "bound " <> show (pretty b) <> " to " <> show (pretty v)

checkingDecl ::
    (TypeChecker tc) =>
    Decl ->
    Int ->
    Env ->
    Context ->
    tc ()
checkingDecl d l ρ γ = emit $ CheckD $ CheckingDecl d l ρ γ

boundType ::
    (TypeChecker tc) =>
    Binding ->
    Value ->
    Value ->
    Int ->
    Context ->
    tc ()
boundType p t v l γ = emit $ CheckD $ BoundType p t v l γ

checkD ::
    (TypeChecker tc) =>
    Int ->
    Decl ->
    Env ->
    Context ->
    tc (Env, Context)
checkD l d@(Decl p a m) ρ γ = do
    checkingDecl d l ρ γ
    checkT l a ρ γ
    let t = eval a ρ
        v = eval m ρ
    check l m t ρ γ
    γ1 <- bindType p t v γ
    boundType p t v l γ1
    trace ("v : " <> show v) $ extendWith ρ γ1 t d
checkD l d@(RDecl p a m) ρ γ = do
    checkingDecl d l ρ γ
    checkT l a ρ γ
    γ_1 <- bindType p t x_l γ
    check (l + 1) m t ρ_1 γ_1
    γ' <- bindType p t v γ
    boundType p t v l γ'
    extendWith ρ γ' t d
  where
    v = eval m (ExtendDecl ρ d)
    t = eval a ρ
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l

extendWith :: TypeChecker tc => Env -> Context -> Value -> Decl -> tc (Env, Context)
extendWith ρ γ t = \case
    d@Decl{body} -> go body t [] [] (ExtendDecl ρ d) γ
    d@RDecl{body} -> go body t [] [] (ExtendDecl ρ d) γ
  where
    bindInEnv bs tys (ρ', γ') (Choice c _a) =
        trace ("bs: " <> show bs <> ", tys: " <> show tys) $ do
            let ctor = B c
            γ'' <- bindType ctor t EUnit γ'
            let ρ'' = ExtendPat ρ' ctor (ECtor c Nothing)
            pure (ρ'', γ'')
    go bod ty bs res ρ_1 γ_1 =
        trace ("body: " <> show bod <> ", ty: " <> show ty) $ case (bod, ty) of
            (Sum cs, _) -> foldM (bindInEnv bs res) (ρ_1, γ_1) cs
            (Abs b rest, EPi t' g) -> trace ("b: " <> show b <> ", rest: " <> show rest <> ", t'" <> show t') $ do
                γ_2 <- bindType b t' x_l γ_1
                go rest (inst g x_l) (b : bs) (t : res) ρ_2 γ_2
              where
                x_l = ENeut $ NV $ NVar 0
                ρ_2 = ExtendPat ρ_1 b x_l
            _ -> pure (ρ, γ)

--

-- ** Check Type Well-Formedness

--
data CheckTEvent
    = CheckingIsType AST Int Env Context
    | CheckedIsType AST Int Env Context
    deriving (Eq, Show)

instance HasLevel CheckTEvent where
    getLevel (CheckingIsType _ l _ _) = l
    getLevel (CheckedIsType _ l _ _) = l

instance Displayable CheckTEvent where
    display (CheckingIsType e _ _ _) = "checking typeness " <> show (pretty e)
    display (CheckedIsType e _ _ _) = "checked typeness " <> show (pretty e)

checkingIsType ::
    (TypeChecker tc) =>
    AST ->
    Int ->
    Env ->
    Context ->
    tc ()
checkingIsType e l ρ γ = emit $ CheckT $ CheckingIsType e l ρ γ

checkedIsType ::
    (TypeChecker tc) =>
    AST ->
    Int ->
    Env ->
    Context ->
    tc ()
checkedIsType e l ρ γ = emit $ CheckT $ CheckedIsType e l ρ γ

checkT ::
    (TypeChecker tc) =>
    Int ->
    AST ->
    Env ->
    Context ->
    tc ()
checkT l e@(Sigma p a b) ρ γ = do
    checkingIsType e l ρ γ
    checkDependentT l p a b ρ γ
    checkedIsType e l ρ γ
checkT l e@(Pi p a b) ρ γ = do
    checkingIsType e l ρ γ
    checkDependentT l p a b ρ γ
    checkedIsType e l ρ γ
checkT l u@U{} ρ γ = checkingIsType u l ρ γ >> checkedIsType u l ρ γ
checkT l a ρ γ = do
    checkingIsType a l ρ γ
    check l a (EU 0) ρ γ
    checkedIsType a l ρ γ

checkDependentT ::
    (TypeChecker tc) =>
    Int ->
    Binding ->
    AST ->
    AST ->
    Env ->
    Context ->
    tc ()
checkDependentT l p a b ρ γ = do
    checkT l a ρ γ
    γ_1 <- bindType p (eval a ρ) x_l γ
    checkT (l + 1) b ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
-- ** Check Type assignment

data CheckEvent
    = CheckingHasType AST Value Int Env Context
    | CheckedHasType AST Value Int Env Context
    deriving (Eq, Show)

instance HasLevel CheckEvent where
    getLevel (CheckingHasType _ _ l _ _) = l
    getLevel (CheckedHasType _ _ l _ _) = l

instance Displayable CheckEvent where
    display (CheckingHasType e v _ _ _) = "checking type of " <> show (pretty e) <> " is " <> show (pretty v)
    display (CheckedHasType e v _ _ _) = "checked type of " <> show (pretty e) <> " is " <> show (pretty v)

checkingHasType ::
    (TypeChecker tc) =>
    AST ->
    Value ->
    Int ->
    Env ->
    Context ->
    tc ()
checkingHasType a t l ρ γ = emit $ Check $ CheckingHasType a t l ρ γ

checkedHasType ::
    (TypeChecker tc) =>
    AST ->
    Value ->
    Int ->
    Env ->
    Context ->
    tc ()
checkedHasType a t l ρ γ = emit $ Check $ CheckedHasType a t l ρ γ

check ::
    (TypeChecker tc) =>
    Int ->
    AST ->
    Value ->
    Env ->
    Context ->
    tc ()
check l a@(Ctor c_i (Just m)) t@(ESum (SumClos (c, ν))) ρ γ = do
    checkingHasType a t l ρ γ
    case choose c c_i of
        Just (Choice _ (Just a_i)) -> do
            check l m (eval a_i ν) ρ γ
            checkedHasType a t l ρ γ
        _ -> throwM $ typingError ("invalid ctor " <> show c_i <> " among " <> show c <> " while expecting type " <> show (pretty t))
check l a@(Ctor c_i Nothing) t@(ESum (SumClos (c, _))) ρ γ = do
    checkingHasType a t l ρ γ
    case choose c c_i of
        Just (Choice _ Nothing) ->
            checkedHasType a t l ρ γ
        _ -> throwM $ typingError ("invalid ctor " <> show c_i <> " among " <> show c <> " while expecting type " <> show (pretty t))
check l a@(Pair m n) ty@(ESig t g) ρ γ = do
    checkingHasType a ty l ρ γ
    check l m t ρ γ
    check l n (inst g (eval m ρ)) ρ γ
    checkedHasType a ty l ρ γ
check l Unit EOne ρ γ = checkingHasType Unit EOne l ρ γ >> checkedHasType Unit EOne l ρ γ
check l One (EU 0) ρ γ = checkingHasType One (EU 0) l ρ γ >> checkedHasType One (EU 0) l ρ γ
check l u@(U n) v@(EU m) ρ γ = do
    checkingHasType u v l ρ γ
    unless (n < m) $
        throwM $ typingError ("inconsistent universe hierarchy: " <> show (pretty u) <> " should be lower than " <> show (pretty v))
    checkedHasType u v l ρ γ
check l a@(Case cs) ty@(EPi esum@(ESum (SumClos (cs', ν))) g) ρ γ = do
    checkingHasType a ty l ρ γ
    when (length cs /= length cs') $
        throwM $ typingError ("number of ctors in case " <> show cs <> " must be same than number in " <> show cs')
    forM_ (zip cs cs') $ \(Clause c_i m_i, Choice c_i' a_i) -> do
        when (c_i /= c_i') $
            throwM $ typingError ("order of ctors in case must be the same same as in definition, found " <> show c_i <> ", expected " <> show c_i')
        case a_i of
            Just a_i' -> check l m_i (EPi (eval a_i' ν) (ClComp g c_i)) ρ γ
            Nothing -> check l m_i (EPi esum (ClComp0 g c_i)) ρ γ
    checkedHasType a ty l ρ γ
check l e@(Sum cs) (EU n) ρ γ = do
    checkingHasType e (EU n) l ρ γ
    forM_ cs (\(Choice _ a_i) -> maybe (pure ()) (\a -> check l a (EU n) ρ γ) a_i)
    checkedHasType e (EU n) l ρ γ
check l e@(Sigma p a b) (EU _) ρ γ = checkDependent l e p a b ρ γ
check l e@(Pi p a b) (EU _) ρ γ = checkDependent l e p a b ρ γ
check l a@(Abs p m) ty@(EPi t g) ρ γ = do
    checkingHasType a ty l ρ γ
    γ_1 <- bindType p t x_l γ
    check (l + 1) m (inst g x_l) ρ_1 γ_1
    checkedHasType a ty l ρ γ
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
check l a@(Let d m) t ρ γ = do
    checkingHasType a t l ρ γ
    (ρ_1, γ_1) <- checkD l d ρ γ
    check l m t ρ_1 γ_1
    checkedHasType a t l ρ γ
check l m t ρ γ = do
    checkingHasType m t l ρ γ
    t' <- checkI l m ρ γ
    let norm = normalize l t :: Normal
        norm' = normalize l t' :: Normal
    unless (same norm norm') $
        throwM $
            typingError $
                "[" <> show l <> "] expr " <> show m
                    <> " does not have type "
                    <> show (pretty t)
                    <> ", inferred type is "
                    <> show (pretty t')
                    <> ", failed to equate normalization "
                    <> show (pretty norm)
                    <> " with "
                    <> show (pretty norm')
                    <> " in env "
                    <> show (pretty ρ)
                    <> " and context "
                    <> show (pretty γ)
    checkedHasType m t l ρ γ

checkDependent ::
    TypeChecker m =>
    Int ->
    AST ->
    Binding ->
    AST ->
    AST ->
    Env ->
    Context ->
    m ()
checkDependent l e p a b ρ γ = do
    checkingHasType e (EU 0) l ρ γ
    check l a (EU 0) ρ γ
    γ_1 <- bindType p (eval a ρ) x_l γ
    check (l + 1) b (EU 0) ρ_1 γ_1
    checkedHasType e (EU 0) l ρ γ
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
-- ** Inter Type of an Expression

data CheckIEvent
    = InferringType AST Int Env Context
    | ResolvingVariable Name Int Env Context
    | InferredType AST Value Int Env Context
    deriving (Eq, Show)

instance HasLevel CheckIEvent where
    getLevel (InferringType _ l _ _) = l
    getLevel (ResolvingVariable _ l _ _) = l
    getLevel (InferredType _ _ l _ _) = l

instance Displayable CheckIEvent where
    display (InferringType e _ _ _) = "inferring type of " <> show (pretty e)
    display (ResolvingVariable n _ _ _) = "resolving " <> show n
    display (InferredType e v _ _ _) = "inferred type of " <> show (pretty e) <> " is " <> show (pretty v)

inferringType ::
    (TypeChecker tc) =>
    AST ->
    Int ->
    Env ->
    Context ->
    tc ()
inferringType a l ρ γ = emit $ CheckI $ InferringType a l ρ γ

resolvingVariable ::
    (TypeChecker tc) =>
    Name ->
    Int ->
    Env ->
    Context ->
    tc ()
resolvingVariable n l ρ γ = emit $ CheckI $ ResolvingVariable n l ρ γ

inferredType ::
    (TypeChecker tc) =>
    AST ->
    Int ->
    Env ->
    Context ->
    Value ->
    tc Value
inferredType a l ρ γ v = emit (CheckI $ InferredType a v l ρ γ) >> pure v

checkI ::
    (TypeChecker tc) =>
    Int ->
    AST ->
    Env ->
    Context ->
    tc Value
checkI l (Var x) ρ γ = do
    resolvingVariable x l ρ γ
    lookupType x γ >>= inferredType (Var x) l ρ γ
checkI l u@(U n) ρ γ = do
    inferredType u l ρ γ (EU $ succ n)
checkI l a@(Ap m n) ρ γ = do
    inferringType a l ρ γ
    typ <- checkI l m ρ γ
    case typ of
        EPi t g -> do
            check l n t ρ γ
            let v = inst g (eval n ρ)
            inferredType a l ρ γ v
        other -> throwM $ typingError $ "expected type of  " <> show (pretty m) <> " to be a product type (Π x : A.f x), but found " <> show (pretty other)
checkI _ (I _) _ _ = pure $ EPrim PrimInt
checkI _ (D _) _ _ = pure $ EPrim PrimDouble
checkI _ (S _) _ _ = pure $ EPrim PrimString
checkI l c@(Ctor c_i Nothing) ρ γ = do
    inferringType c l ρ γ
    typ <- lookupCtor c_i ρ γ
    case typ of
        EPi{} -> throwM $ typingError $ "ctor needs " <> unpack c_i <> " needs one argument"
        _ -> inferredType c l ρ γ typ
checkI l c@(Ctor c_i (Just a)) ρ γ = do
    inferringType c l ρ γ
    typ <- lookupCtor c_i ρ γ
    case typ of
        EPi t g -> do
            check l a t ρ γ
            let v = inst g (eval a ρ)
            inferredType a l ρ γ v
        other -> throwM $ typingError $ "expected type of ctor lookup " <> unpack c_i <> " to be a product type (Π x : A.f x), but found " <> show (pretty other)
checkI l e ρ γ =
    throwM $ typingError $ "[" <> show l <> "] cannot infer type of " <> show e <> " in env " <> show (pretty ρ) <> " and context " <> show (pretty γ)

{- | Lookup a constructor's definition in the current environment.
 We look through all the environment's bindings' definitions to find a
 `Sum` instance where the given constructor is defined.
-}
lookupCtor :: (TypeChecker tc) => Name -> Env -> Context -> tc Value
lookupCtor c_i (ExtendDecl ρ (Decl _ _ e)) γ =
    trace ("lookup in " <> show ρ) $
        selectCtor c_i e ρ γ
lookupCtor c_i (ExtendDecl ρ (RDecl _ _ e)) γ =
    trace ("lookup in " <> show ρ) $
        selectCtor c_i e ρ γ
lookupCtor c_i (ExtendPat ρ _ _) γ = lookupCtor c_i ρ γ
lookupCtor c_i EmptyEnv _ = throwM $ typingError $ "cannot find constructor " <> show c_i

selectCtor :: TypeChecker tc => Text -> AST -> Env -> Context -> tc Value
selectCtor c_i e@(Sum cs) ρ γ =
    trace ("select " <> unpack c_i <> " in " <> show cs <> " env=" <> show ρ) $
        case choose cs c_i of
            Just (Choice _ Nothing) ->
                pure $ eval e ρ
            Just (Choice _ (Just a)) ->
                -- TODO: need a unique name here
                pure $ eval (Pi (B "u") a e) ρ
            _ -> lookupCtor c_i ρ γ
selectCtor c_i ab@(Abs b e) ρ γ =
    trace ("select Abs " <> unpack c_i <> " in " <> show ab) $
        selectCtor c_i e ρ_1 γ
  where
    x_l = ENeut $ NV $ NVar 0
    ρ_1 = ExtendPat ρ b x_l
selectCtor c_i ast EmptyEnv _ = error $ "cannot select " <> Text.unpack c_i <> " in " <> show ast
selectCtor c_i _ ρ γ = lookupCtor c_i ρ γ
-- * Programs

{- | Top-level evaluator
 Load an AST into given `Env` and `Context`, returning an extended
 `Env` and `Context` with declarations bound
-}
loadProgram ::
    (TypeChecker tc) =>
    AST ->
    Env ->
    Context ->
    tc (Env, Context)
loadProgram (Let d r) ρ γ = do
    (ρ', γ') <- checkD 0 d ρ γ
    loadProgram r ρ' γ'
loadProgram e ρ γ = do
    check 0 e EOne ρ γ
    pure (ρ, γ)
