{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Monad.State
import           Data.Text           (pack, unpack)
import           Quizz
import           System.IO

q1 =  Question (OpenQuestion "What is your name ?" "Sir Arthur" Nothing) Just
q2 =  Question (OpenQuestion "What is your quest?" "To seek the Holy Grail" Nothing) Just
q3 =  Question (QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 0 Nothing) (Just . read . unpack)
q4 =  Question (QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] 1 Nothing) (Just . read . unpack)
q5 =  Question (Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) 35 Nothing) (Just . read . unpack)


survey =
  Quizz [] q1 [ q2, q3, q4 ]


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  result <- quizz survey
  putStrLn $ show result
  case bridgeKeeperAssessment arthur result of
    CanCross _ -> putStrLn $ "You can cross the bridge " <> show arthur
    IsDoomed _ -> putStrLn $ "You shall fall into the Chasm of Death, " <> show arthur
  where
    arthur = Knight "Arthur" (const Nothing)

    quizz cur@Quizz{currentQuestion} = do
      putStr (show currentQuestion)
      answer <- getLine
      let updated = answerQuestion (arthur { responses = const (pure $ pack answer) }) currentQuestion cur
      case nextQuestions updated of
        [] -> pure updated
        _  -> quizz updated
