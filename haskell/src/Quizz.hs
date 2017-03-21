module Quizz (Quizz(..), Question(..), Knight(..),
              answers, bridgeKeeperAssessment)
where

import           Data.Text


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

data Knight = Knight { name    :: Text
                     , responses :: Question -> Response
                     }

instance Show Knight where
  show Knight{..} = "Sir " ++ show name

instance Eq Knight where
  Knight n1 _ == Knight n2 _ = n1 == n2
  
data Fate = CanCross Knight
          | IsDoomed Knight
          deriving (Eq, Show)

answers :: Knight -> Quizz -> Quizz
answers _ q = q

bridgeKeeperAssessment :: Knight -> Quizz -> Fate
bridgeKeeperAssessment knight _ = IsDoomed knight 
