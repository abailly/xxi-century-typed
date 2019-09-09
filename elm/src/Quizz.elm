module Quizz exposing (main)

import Html exposing (program)
import Quizz.Types exposing (..)
import Quizz.Update exposing (update)
import Quizz.View exposing (view)


{-| Main
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )
