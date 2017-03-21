module Survey where

import           Data.Text


data Survey = Survey { previousQuestions :: [ Question ]
                     , currentQuestion   :: Question
                     , nextQuestions     :: [ Question ]
                     }
              deriving (Show)

data Question = QCM { question   :: Text
                    , qcmOptions :: [ Text ]
                    , response   :: Maybe Int
                    }
              | Grade { question   :: Text
                      , gradeRange :: (Int, Int)
                      , response   :: Maybe Int
                      }
              | OpenQuestion { question :: Text
                             , response :: Maybe Text
                             }
              deriving (Show)
