{-# LANGUAGE LambdaCase #-}
module QuizzSpec  where

import           Data.Text       (unpack)
import           Quizz
import           Test.Hspec
import           Test.QuickCheck

possibleQuestions :: Gen Question
possibleQuestions =
  elements [ Question (OpenQuestion "What is your name ?" "Sir Lancelot" Nothing) Just
           , Question (OpenQuestion "What is your quest?" "To seek the HolyGrail" Nothing) Just
           , Question (QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 0 Nothing) (Just . read . unpack)
           , Question (QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] 1 Nothing) (Just . read . unpack)
           , Question (Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) 35 Nothing) (Just . read . unpack)
           ]

instance Arbitrary Question where
  arbitrary = possibleQuestions

instance Arbitrary Quizz where
  arbitrary = do
    questions <- vectorOf 3 possibleQuestions
    return $ Quizz [] (head questions) questions

lancelot = Knight "Lancelot"
           (\case
               "What is your name ?"                                   -> Just "Sir Lancelot"
               "What is your quest?"                                   -> Just  "To seek the Holy Grail"
               "What is your  favourite colour?"                       -> Just "0"
               "What is the capital of Assyria?"                       -> Just "1"
               "What is the air-speed velocity of an unladen swallow?" -> Just "35"
               _ -> Nothing
               )

robin = Knight "Robin"
           (\case
               "What is your name ?"                                   -> Just "Sir Robin"
               "What is your quest?"                                   -> Just "To seek the Holy Grail"
               "What is your  favourite colour?"                       -> Just "1"
               "What is the capital of Assyria?"                       -> Just "1"
               "What is the air-speed velocity of an unladen swallow?" -> Just "35"
               _ -> Nothing
               )

instance Arbitrary Knight where
  arbitrary = elements [ lancelot, robin ]

-- from Monty Python's Holy Grail
-- see http://www.retrojunk.com/content/child/quote/page/210/monty-python-and-the-holy-grail
-- https://youtu.be/y2R3FvS4xr4
allows_crossing_bridge_when_3_answers_are_correct :: Quizz -> Knight -> Property
allows_crossing_bridge_when_3_answers_are_correct quizz knight =
  let result  = knight `answers` quizz
      outcome = bridgeKeeperAssessment knight result
      isCorrect (Question q _) = isCorrectAnswer q
  in  all isCorrect (previousQuestions result) ==> outcome == CanCross knight

spec :: Spec
spec = describe "Bridge of Death" $ do

  it "answer is correct if it matches expected" $ do
    isCorrectAnswer (OpenQuestion "What is your name ?" "foo" (Just "foo")) `shouldBe` True
    isCorrectAnswer (QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] 0 (Just 0))
      `shouldBe` True
    isCorrectAnswer (Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) 35 (Just 35))
      `shouldBe` True

  it "allows crossing the bridge if three answers are correct" $ property $
    allows_crossing_bridge_when_3_answers_are_correct
