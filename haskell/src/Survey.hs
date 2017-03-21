module Survey where

import           Data.Text


data Survey = Survey { previousQuestions :: [ Question ]
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
