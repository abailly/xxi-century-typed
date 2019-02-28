module Quizz.Update exposing (update)

import Quizz.Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case state of
        QuizzCompleted _ ->
            state ! []

        QuizzInProgress quizz ->
            case msg of
                UpdateResponse s ->
                    QuizzInProgress { quizz | currentResponse = s } ! []

                SubmitResponse r ->
                    let
                        answered =
                            updateAnswer r quizz.current

                        answeredQuestions =
                            answered :: quizz.pastQuestions
                    in
                    case quizz.nextQuestions of
                        [] ->
                            QuizzCompleted { answers = answeredQuestions } ! []

                        q :: qs ->
                            QuizzInProgress
                                { quizz
                                    | pastQuestions = answeredQuestions
                                    , current = q
                                    , nextQuestions = qs
                                }
                                ! []

                NoOp ->
                    state ! []
