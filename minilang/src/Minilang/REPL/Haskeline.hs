{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL.Haskeline where

import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (
    isPrefixOf,
    isSuffixOf,
 )
import Data.Text (pack, unpack)
import Data.Text.IO as Text
import Minilang.Pretty (render)
import Minilang.REPL.Run (handleCommand)
import Minilang.REPL.Types
import Minilang.Type
import System.Console.Haskeline (
    Completion (Completion, replacement),
    CompletionFunc,
    InputT,
    getInputLineWithInitial,
    outputStrLn,
 )
import System.Directory (listDirectory)

-- * Haskeline REPL

newtype Haskeline a = Haskeline {runHaskeline :: StateT REPLEnv (InputT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState REPLEnv)

hoist :: InputT IO a -> Haskeline a
hoist m = Haskeline $ StateT $ \s -> m >>= \a -> pure (a, s)

instance MonadREPL Haskeline where
    input = do
        i <- hoist (getInputLineWithInitial "λΠ> " ("", ""))
        case i of
            Nothing -> pure EOF
            Just (pack -> t) -> pure $ interpret t
    output = hoist . outputStrLn . unpack . render
    prompt = pure ()
    load = fmap (first FileError) . hoist . lift . try . Text.readFile . unpack

    getEnv = get
    setEnv = put

completion :: CompletionFunc IO
completion = completion' . first reverse

data CompleteWith
    = SetUnset String
    | LoadFile
    | AllCommands

completion' :: CompletionFunc IO
completion' (inputString, _) =
    case matchedCompletionRule inputString of
        SetUnset com : _ -> setUnsetCompletion com
        LoadFile : _ -> completeLoadWithFiles (drop 6 inputString)
        _ -> allCommandCompletions
  where
    matchedCompletionRule prefix =
        snd
            <$> filter
                (($ prefix) . fst)
                [ ((":set" `isPrefixOf`), SetUnset ":set")
                , ((":unset" `isPrefixOf`), SetUnset ":unset")
                , ((":load" `isPrefixOf`), LoadFile)
                , (const True, AllCommands)
                ]

    completeLoadWithFiles prefix = do
        files <- filter (prefix `isPrefixOf`) . filter (".mtt" `isSuffixOf`) <$> listDirectory "."
        pure ("", fmap (\fp -> Completion (":load " <> fp) fp True) files)

    setUnsetCompletion com =
        pure
            ( ""
            , filter
                ((inputString `isPrefixOf`) . replacement)
                [ Completion (com <> " step") "step - Step through evaluation process, displaying each rule application" True
                , Completion (com <> " debug") "debug - Debug parsing of input" True
                ]
            )

    allCommandCompletions =
        pure
            ( ""
            , filter
                ((inputString `isPrefixOf`) . replacement)
                [ Completion ":quit" ":quit - Quit minilang REPL" False
                , Completion ":env" ":env - Dump current environment and context (types and values)" False
                , Completion ":clear" ":clear - Clear environment" False
                , Completion ":load" ":load - Load a file into the REPL" True
                , Completion ":set" ":set - Set some properties of the REPL" True
                , Completion ":unset" ":unset - Unset some properties of the REPL" True
                , Completion ":help" ":help - Provide help on commands" True
                ]
            )

instance MonadCatch Haskeline where
    Haskeline m `catch` f = Haskeline $ m `catch` \e -> runHaskeline (f e)

instance MonadThrow Haskeline where
    throwM e = throw e

instance TypeChecker Haskeline where
    emit e = printE e

printE ::
    Event -> Haskeline ()
printE e = do
    let prefix depth = replicate (2 * depth) ' ' <> display e <> " > "
        handleStep depth = do
            c <- hoist $ getInputLineWithInitial (prefix depth) ("", "")
            case c of
                Nothing ->
                    pure ()
                Just (pack -> t) ->
                    case interpret t of
                        Com cmd -> handleCommand cmd
                        _ -> pure ()
        indent = modify $ \repl -> repl{curIndent = curIndent repl + 1}
        dedent = modify $ \repl -> repl{curIndent = curIndent repl - 1}
    step <- gets stepTypeCheck
    curIndent <- gets curIndent
    if step
        then case e of
            (CheckD CheckingDecl{}) -> handleStep curIndent >> indent
            (CheckD BoundType{}) -> handleStep (curIndent - 1) >> dedent
            (CheckT CheckingIsType{}) -> handleStep curIndent >> indent
            (CheckT CheckedIsType{}) -> handleStep (curIndent - 1) >> dedent
            (Check CheckingHasType{}) -> handleStep curIndent >> indent
            (Check CheckedHasType{}) -> handleStep (curIndent - 1) >> dedent
            (CheckI InferringType{}) -> handleStep curIndent >> indent
            (CheckI ResolvingVariable{}) -> handleStep curIndent >> indent
            (CheckI InferredType{}) -> handleStep (curIndent - 1) >> dedent
        else pure ()
