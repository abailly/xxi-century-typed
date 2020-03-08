{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module HStore.FileOpsSpec(spec) where

import           Control.Concurrent.Async
import           Control.Exception        (bracket)
import           Data.Either
import           Data.Either
import           Data.Functor
import           Data.IORef
import           Data.Serialize
import           Data.Text                (Text, pack)
import           GHC.Generics
import           HStore
import           HStore.FileOps
import           Prelude                  hiding (init)
import           System.Directory
import           System.FilePath          ((</>))
import           System.IO                (hClose)
import           System.Posix.Temp        (mkstemp)
import           Test.Hspec
import           Test.QuickCheck          as Q
import           Test.QuickCheck.Monadic  as Q

newtype Add = Add Int
  deriving (Eq, Show)

instance Arbitrary Add where
  arbitrary = Add <$> choose (1,100)

newtype Added = Added Int
  deriving (Eq, Show, Generic)

instance Serialize Added
instance Versionable Added

mkEvent :: Add -> IO (Either () Added)
mkEvent (Add i) = pure $ Right (Added i)

data Stored = Stored Added
    | Failed Text
    deriving (Eq, Show)

mkResult :: Either () (StorageResult Added) -> IO Stored
mkResult (Right (WriteSucceed a)) = pure $ Stored a
mkResult (Right err)              = pure $ Failed (pack $ show err)
mkResult (Left err)               = pure $ Failed (pack $ show err)

storeAdd :: FileStorage -> Add -> IO (Either Text Added)
storeAdd st a = do
  result <- store st (mkEvent a) mkResult
  case result of
    WriteSucceed (Stored a) -> pure $ Right a
    err                     -> pure $ Left $ pack $ show err

prop_persistentStateSerializesConcurrentWrites :: StorageOptions -> [[ Add ]] -> Property
prop_persistentStateSerializesConcurrentWrites storageOpts commands = collect (length commands) $ monadicIO $ do
  (errs, evs) <- Q.run $ withStorage storageOpts $ \ st -> do
    void $ reset st
    evs <- partitionEithers . concat <$> mapConcurrently (mapM (storeAdd st)) commands
    return evs

  monitor (counterexample $ show commands)
  assert $ length evs == length (concat commands)

  LoadSucceed evs' <- Q.run $ withStorage storageOpts load

  -- We check all events returned from actions are stored but they may be in different orders
  -- although all command execution and writes are serialized, it is possible the events be
  -- returned to this test thread in different orders
  assert $ all (`elem` evs') evs && all (`elem` evs) evs'

storageOpts = StorageOptions { storageFilePath = "test.store"
                             , storageVersion = Version 1
                             , storageQueueSize = 100
                             }

temporaryStorage = bracket newTempStorage deleteStorage
  where
    newTempStorage = do
      dir <- getTemporaryDirectory
      (f, h) <- mkstemp (dir </> "store.test")
      hClose h
      pure $ StorageOptions { storageFilePath = f
                            , storageVersion = Version 1
                            , storageQueueSize = 100
                            }
    deleteStorage StorageOptions{storageFilePath} = removeFile storageFilePath



spec :: Spec
spec = around temporaryStorage $ describe "State Effect" $ do

  it "should serialize concurrent writes to Model" $ \ storageOpts -> property (prop_persistentStateSerializesConcurrentWrites storageOpts)
