module Main where

import           Quizz

q1 = OpenQuestion "What is your name ?" Nothing

q2 = OpenQuestion "What is your quest?" Nothing

q3 = QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] Nothing

q4 = QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] Nothing

q5 = Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) Nothing

survey =
  Quizz [] q1 [ q1, q2, q3, q4 ]


main :: IO ()
main = putStrLn ""

