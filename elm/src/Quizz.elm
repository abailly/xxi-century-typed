module Quizz exposing (main)

import Quizz.Types exposing (..)
import Quizz.Update exposing (update)
import Quizz.View exposing (view)
import Html exposing (program)


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
    initialModel ! []
