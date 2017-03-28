module Quizz (Quizz(..), Question(..), Knight(..), Fate(..), Response(..),
              Expected(..),
              answers, bridgeKeeperAssessment, isCorrectAnswer)
where

import           Data.Text (Text, unpack)

newtype Question where
  Question :: (forall q . (Questionable q, Show q) => q ) -> Question

instance Show Question where
  show (Question q) = show q
    
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

class Questionable q where
  type Answer q :: *

  question :: q -> Text
  expected :: q -> Answer q
  response :: q -> Maybe (Answer q)

data QCM = QCM { _question   :: Text
               , _qcmOptions :: [ Text ]
               , _expected   :: Int
               , _response   :: Maybe Int
               }
              deriving (Show)

instance Questionable QCM where
  type Answer QCM = Int

  question (QCM q _ _ _) = q
  expected (QCM _ _ e _) = e
  response (QCM _ _ _ r) = r

data Grade = Grade { _question   :: Text
                   , _gradeRange :: (Int, Int)
                   , _expected   :: Expected
                   , _response   :: Maybe Response
                   }
              deriving (Show)

instance Questionable Grade where

data OpenQuestion = OpenQuestion { _question :: Text
                                 , _expected :: Expected
                                 , _response :: Maybe Response
                                 }
                  deriving (Show)

instance Questionable OpenQuestion where
  type Answer OpenQuestion = Text

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

-- we can see here the types are not precise enough: we need to do a test at runtime
-- through pattern matching to ensure we check the correct answer type for the given
-- question
isCorrectAnswer :: Question -> Bool
isCorrectAnswer (Question q) = Just (expected q) == response q
-- isCorrectAnswer (OpenQuestion _ (Open f)   (Just (FreeText t))) = f t
-- isCorrectAnswer (QCM _ _        (Closed e) (Just r@(Option _))) = e == r
-- isCorrectAnswer (Grade _ _      (Closed e) (Just r@(Graded _))) = e == r
-- isCorrectAnswer _                                               = False

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
bridgeKeeperAssessment knight quizz =
  if all isCorrectAnswer (previousQuestions $ knight `answers` quizz)
  then CanCross knight
  else IsDoomed knight
