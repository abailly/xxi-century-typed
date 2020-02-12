{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL.Haskeline where


import           Control.Exception                       (throw)
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans                     (lift)
import           Data.Bifunctor
import           Data.List                               (isPrefixOf,
                                                          isSuffixOf)
import           Data.Text                               (pack, unpack)
import           Data.Text.IO                            as Text
import           Minilang.REPL.Types
import           Minilang.Type
import           System.Console.Haskeline                (Completion (Completion, replacement),
                                                          CompletionFunc,
                                                          InputT,
                                                          getInputLineWithInitial,
                                                          outputStrLn)
import qualified System.Console.Haskeline.MonadException as Exc
import           System.Directory                        (listDirectory)


-- * Haskeline REPL


newtype Haskeline a = Haskeline { runHaskeline :: StateT REPLEnv (InputT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState REPLEnv)

hoist :: InputT IO a -> Haskeline a
hoist m = Haskeline $ StateT $ \ s -> m >>= \ a -> pure (a,s)

instance MonadREPL Haskeline where
  input  = do
    i <- hoist (getInputLineWithInitial "λΠ> " ("",""))
    case i of
      Nothing          -> pure EOF
      Just (pack -> t) -> pure $ interpret t
  output = hoist . outputStrLn . unpack
  prompt = pure ()
  load   = fmap (bimap FileError id) . hoist . lift . try . Text.readFile . unpack

  getEnv = get
  setEnv = put

completion :: CompletionFunc IO
completion = completion' . first reverse

data CompleteWith = SetUnset String
    | LoadFile
    | AllCommands

completion' :: CompletionFunc IO
completion' (inputString ,_) =
  case matchedCompletionRule inputString of
    SetUnset com:_ -> setUnsetCompletion com
    LoadFile:_     -> completeLoadWithFiles (drop 6 inputString)
    _              -> allCommandCompletions

  where
    matchedCompletionRule prefix =
      snd <$> filter (($ prefix) . fst)
      [ ((":set" `isPrefixOf`), SetUnset ":set")
      , ((":unset" `isPrefixOf`), SetUnset ":unset")
      , ((":load" `isPrefixOf`), LoadFile)
      , (const True, AllCommands)
      ]

    completeLoadWithFiles prefix = do
      files <- filter (prefix `isPrefixOf`) . filter (".mtt" `isSuffixOf`) <$> listDirectory "."
      pure ("", fmap (\ fp -> Completion (":load " <> fp) fp True) files)

    setUnsetCompletion com =
      pure ("", filter ((inputString `isPrefixOf`) . replacement)
                [ Completion (com <>" step")  "step - Step through evaluation process, displaying each rule application" True
                , Completion (com <> " debug")  "debug - Debug parsing of input" True
                ])

    allCommandCompletions =
      pure ("", filter ((inputString `isPrefixOf`) . replacement)
                [ Completion ":quit"  ":quit - Quit minilang REPL" False
                , Completion ":env"  ":env - Dump current environment and context (types and values)" False
                , Completion ":clear"  ":clear - Clear environment" False
                , Completion ":load"  ":load - Load a file into the REPL" True
                , Completion ":set"  ":set - Set some properties of the REPL" True
                , Completion ":unset"  ":unset - Unset some properties of the REPL" True
                , Completion ":help"  ":help - Provide help on commands" True
                ])

instance MonadThrow (InputT IO) where
  throwM = Exc.throwIO

instance MonadCatch (InputT IO) where
  m `catch` f = m `Exc.catch` f

instance MonadCatch Haskeline where
  Haskeline m `catch` f =  Haskeline $ m `catch` \ e -> runHaskeline (f e)

instance MonadThrow Haskeline where
  throwM e = throw e

instance TypeChecker Haskeline where
  emit e = get >>= hoist . (printE e) >>= put

printE
  :: Event -> REPLEnv -> InputT IO REPLEnv
printE e env@REPLEnv{..} =
  let
    prefix depth = replicate (2 * depth) ' ' <> display e
    doPrint depth = void (getInputLineWithInitial (prefix depth) ("", ""))
  in
    if stepTypeCheck
    then case e of
      (CheckD CheckingDecl{})      -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckD BoundType{})         -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (CheckT CheckingIsType{})    -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckT CheckedIsType {})    -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (Check  CheckingHasType{})   -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (Check  CheckedHasType{} )   -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (CheckI InferringType {})    -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckI ResolvingVariable{}) -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckI InferredType{})      -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
    else pure env
