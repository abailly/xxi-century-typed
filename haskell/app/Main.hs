module Main where

import           Data.Text (unpack)
import           Quizz

q1 =  Question (OpenQuestion "What is your name ?" "Sir Arthur" Nothing) Just
q2 =  Question (OpenQuestion "What is your quest?" "To seek the Holy Grail" Nothing) Just
q3 =  Question (QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 0 Nothing) (Just . read . unpack)
q4 =  Question (QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] 1 Nothing) (Just . read . unpack)
q5 =  Question (Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) 35 Nothing) (Just . read . unpack)


survey =
  Quizz [] q1 [ q1, q2, q3, q4 ]


main :: IO ()
main = putStrLn ""

