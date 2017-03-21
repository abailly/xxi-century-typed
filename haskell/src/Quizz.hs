module Quizz (Quizz(..), Question(..), Knight(..), Fate(..), Response(..),
              answers, bridgeKeeperAssessment, isCorrectAnswer)
where

import           Data.Text
import           Test.QuickCheck

data Quizz = Quizz { previousQuestions :: [ Question ]
                   , currentQuestion   :: Question
                   , nextQuestions     :: [ Question ]
                   }
           deriving (Show)

data Response = Graded Int
              | Option Int
              | FreeText Text
              deriving  (Show)

data Question = QCM { question   :: Text
                    , qcmOptions :: [ Text ]
                    , response   :: Maybe Response
                    }
              | Grade { question   :: Text
                      , gradeRange :: (Int, Int)
                      , response   :: Maybe Response
                      }
              | OpenQuestion { question :: Text
                             , response :: Maybe Response
                             }
              deriving (Show)

data Knight = Knight { name      :: Text
                     , responses :: Question -> Response
                     }

instance Show Knight where
  show Knight{..} = "Sir " ++ unpack name

instance Eq Knight where
  Knight n1 _ == Knight n2 _ = n1 == n2

data Fate = CanCross Knight
          | IsDoomed Knight
          deriving (Eq, Show)

isCorrectAnswer :: Question -> Bool
isCorrectAnswer _ = False

answers :: Knight -> Quizz -> Quizz
answers _ q = q

bridgeKeeperAssessment :: Knight -> Quizz -> Fate
bridgeKeeperAssessment knight _ = IsDoomed knight
