module QuizzSpec  where

import           Quizz
import           Test.Hspec
import           Test.QuickCheck


allows_crossing_bridge_when_3_answers_are_correct :: Quizz -> Knight -> Bool
allows_crossing_bridge_when_3_answers_are_correct quizz knight =
  let result  = knight `answers` quizz
      outcome = bridgeKeeperAssessment result
  in  all isCorrectAnswer result ==> outcome == CanCross knight



spec :: Spec
spec = describe "Bridge of Death" $ do

  it "allows crossing the bridge if three answers are correct" $ property $
    allows_crossing_bridge_when_3_answers_are_correct
