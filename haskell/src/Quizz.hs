module Quizz (Quizz(..), Question(..), QCM(..), OpenQuestion(..), Grade(..), Knight(..), Fate(..), Response(..),
              Expected(..),
              answers, bridgeKeeperAssessment, isCorrectAnswer)
where

import           Data.Text (Text, unpack)

data Question where
  Question :: (Questionable q) => q -> (Text -> Maybe (Answer q)) -> Question

instance Show Question where
  show (Question q _) = unpack $ question q

data Quizz = Quizz { previousQuestions :: [ Question ] -- there should be a concept of answered questions...
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


class (Eq (Answer q)) => Questionable q where
  type Answer q :: *

  question :: q -> Text
  expected :: q -> Answer q
  response :: q -> Maybe (Answer q)
  answered :: q -> Maybe (Answer q) -> q

  isCorrectAnswer :: q -> Bool
  isCorrectAnswer q = Just (expected q) == response q

data QCM = QCM { _question   :: Text
               , _qcmOptions :: [ Text ]
               , _expected   :: Int
               , _response   :: Maybe Int
               }
              deriving (Show, Eq)

instance Questionable QCM where
  type Answer QCM = Int
  question (QCM q _ _ _) = q
  expected (QCM _ _ e _) = e
  response (QCM _ _ _ r) = r

  answered qcm r = qcm { _response = r }


data Grade = Grade { _question   :: Text
                   , _gradeRange :: (Int, Int)
                   , _expected   :: Int
                   , _response   :: Maybe Int
                   }
              deriving (Show)

instance Questionable Grade where
  type Answer Grade = Int
  question (Grade q _ _ _) = q
  expected (Grade _ _ e _) = e
  response (Grade _ _ _ r) = r
  answered grade r = grade { _response = r }

data OpenQuestion = OpenQuestion { _question :: Text
                                 , _expected :: Text
                                 , _response :: Maybe Text
                                 }
                  deriving (Show)

instance Questionable OpenQuestion where
  type Answer OpenQuestion = Text
  question (OpenQuestion q _ _) = q
  expected (OpenQuestion _ e _) = e
  response (OpenQuestion _ _ r) = r
  answered open r = open { _response = r }

data Knight = Knight { name      :: Text
                     , responses :: Question -> Maybe Text
                     }

instance Show Knight where
  show Knight{..} = "Sir " ++ unpack name

instance Eq Knight where
  Knight n1 _ == Knight n2 _ = n1 == n2

data Fate = CanCross Knight
          | IsDoomed Knight
          deriving (Eq, Show)


answerQuestion :: Knight -> Question -> Quizz -> Quizz
answerQuestion Knight{..} quest quizz =
  update quizz quest
  where
    update :: Quizz -> Question -> Quizz
    update Quizz{..} (Question q a) = let r = (responses quest >>= a)
                                      in quizz { previousQuestions = Question (answered q r) a : previousQuestions
                                               , currentQuestion = head nextQuestions
                                               , nextQuestions = tail nextQuestions
                                               }

answers :: Knight -> Quizz -> Quizz
answers k q =
 foldr (answerQuestion k) q (nextQuestions q)

bridgeKeeperAssessment :: Knight -> Quizz -> Fate
bridgeKeeperAssessment knight quizz =
  if all answersAreCorrect (previousQuestions $ knight `answers` quizz)
  then CanCross knight
  else IsDoomed knight
  where
    answersAreCorrect (Question q _) = isCorrectAnswer q
