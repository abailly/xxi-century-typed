module QuizzSpec  where

import           Quizz
import           Test.Hspec
import           Test.QuickCheck

possibleQuestions =
  elements [ OpenQuestion "What is your name ?" Nothing
           , OpenQuestion "What is your quest?" Nothing
           , QCM "What is your favourite colour?" [ "blue", "yellow", "green", "Don't know"] Nothing
           , QCM "What is the capital of Assyria?" [ "Babylone", "Ninive", "Ur", "Don't know" ] Nothing
           , Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) Nothing
           ]

instance Arbitrary Quizz where
  arbitrary = do
    questions <- vectorOf 3 possibleQuestions
    return $ Quizz [] (head questions) questions

lancelot = Knight "Lancelot"
           (\case
               OpenQuestion "What is your name ?"     _ -> FreeText "Sir Lancelot"
               OpenQuestion "What is your quest?"     _ -> FreeText "To seek the Holy Grail"
               QCM  "What is your  favourite colour?" _ _ -> Option 0
               QCM  "What is the capital of Assyria?" _ _ -> Option 1
               Grade "What is the air-speed velocity of an unladen swallow?" (0, 150) _ -> Graded 35
               )
instance Arbitrary Knight where
  arbitrary = elements [ lancelot ]

-- from Monty Python's Holy Grail
-- see http://www.retrojunk.com/content/child/quote/page/210/monty-python-and-the-holy-grail
-- https://youtu.be/y2R3FvS4xr4
allows_crossing_bridge_when_3_answers_are_correct :: Quizz -> Knight -> Property
allows_crossing_bridge_when_3_answers_are_correct quizz knight =
  let result  = knight `answers` quizz
      outcome = bridgeKeeperAssessment knight result
  in  all isCorrectAnswer (previousQuestions result) ==> outcome == CanCross knight



spec :: Spec
spec = describe "Bridge of Death" $ do

  it "allows crossing the bridge if three answers are correct" $ property $
    allows_crossing_bridge_when_3_answers_are_correct
