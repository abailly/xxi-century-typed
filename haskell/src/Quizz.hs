module Quizz (Quizz(..), Question(..), Knight(..), Fate(..), Response(..),
              Expected(..),
              answers, bridgeKeeperAssessment, isCorrectAnswer)
where

import           Data.Text (Text, unpack)

data Quizz = Quizz { previousQuestions :: [ Question ]
                   , currentQuestion   :: Question
                   , nextQuestions     :: [ Question ]
                   }
           deriving (Show)

data Response = Graded Int
              | Option Int
              | FreeText Text
              deriving  (Eq, Show)

data Expected =
  Open (Text -> Bool)
  | Closed Response

instance Show Expected where
  show (Open _  ) = "Open ended"
  show (Closed r) = "Closed " ++ show r

data Question = QCM { question   :: Text
                    , qcmOptions :: [ Text ]
                    , expected   :: Expected
                    , response   :: Maybe Response
                    }
              | Grade { question   :: Text
                      , gradeRange :: (Int, Int)
                      , expected   :: Expected
                      , response   :: Maybe Response
                      }
              | OpenQuestion { question :: Text
                             , expected :: Expected
                             , response :: Maybe Response
                             }
              deriving (Show)

data Knight = Knight { name      :: Text
                     , responses :: Question -> Maybe Response
                     }

instance Show Knight where
  show Knight{..} = "Sir " ++ unpack name

instance Eq Knight where
  Knight n1 _ == Knight n2 _ = n1 == n2

data Fate = CanCross Knight
          | IsDoomed Knight
          deriving (Eq, Show)

isCorrectAnswer :: Question -> Bool
isCorrectAnswer (OpenQuestion _ (Open f)   (Just (FreeText t))) = f t
isCorrectAnswer (QCM _ _        (Closed e) (Just r@(Option _))) = e == r
isCorrectAnswer (Grade _ _      (Closed e) (Just r@(Graded _))) = e == r
isCorrectAnswer _                                               = False

answerQuestion :: Knight -> Question -> Quizz -> Quizz
answerQuestion Knight{..} question quizz =
  update quizz (responses question)
  where
    update Quizz{..} r = quizz { previousQuestions = question { response = r } : previousQuestions
                               , currentQuestion = head nextQuestions
                               , nextQuestions = tail nextQuestions
                               }

answers :: Knight -> Quizz -> Quizz
answers k q =
 foldr (answerQuestion k) q (nextQuestions q)

bridgeKeeperAssessment :: Knight -> Quizz -> Fate
bridgeKeeperAssessment knight _ = IsDoomed knight
