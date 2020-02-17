{-# LANGUAGE ScopedTypeVariables #-}
module Minilang.JSON where

import           Data.Aeson
import           Text.Parsec.Error
import           Text.Parsec.Pos   (newPos, sourceColumn, sourceLine)

instance ToJSON ParseError where
  toJSON err = object [ "line" .= sourceLine (errorPos err)
                      , "column" .= sourceColumn (errorPos err)
                      , "messages" .= fmap messageString (errorMessages err)
                      ]

instance FromJSON ParseError where
  parseJSON = withObject "ParseError" extractError
    where
      extractError obj = do
        l <- obj .: "line"
        c <- obj .: "column"
        (msgs :: [String]) <- obj .:  "messages"
        pure $ foldr addErrors (newErrorUnknown (newPos "" l c)) msgs
      addErrors msg = addErrorMessage (Message msg)
