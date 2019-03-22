{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.IO where

import qualified Data.ByteString                       as BS
import           Data.Text.Encoding                    (decodeUtf8With,
                                                        encodeUtf8)
import           Data.Text.Encoding.Error              (lenientDecode)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Minilang.Env
import           Minilang.Eval                         hiding (rho)
import           Minilang.Parser
import           Minilang.Type
import           System.IO                             (Handle)


-- | Read an `AST` from @hin@ handle, evaluate it and dump the result
-- on @hout@.
runEval :: Handle -> Handle -> IO ()
runEval hin hout = do
  programText <- decodeUtf8With lenientDecode <$> BS.hGetContents hin
  let ast = parseProgram False programText
      ρ = EmptyEnv
      γ = EmptyContext
  (ρ', _) <- loadProgram ast ρ γ
  let
    val = eval ast ρ'

  BS.hPut hout (encodeUtf8 (renderStrict $ layoutPretty defaultLayoutOptions $ pretty val) <> "\n")
