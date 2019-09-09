module Quizz.Update exposing (update)

import Quizz.Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case state of
        QuizzCompleted _ ->
            ( state
            , Cmd.none
            )

        QuizzInProgress quizz ->
            case msg of
                UpdateResponse s ->
                    ( QuizzInProgress { quizz | currentResponse = s }
                    , Cmd.none
                    )

                SubmitResponse r ->
                    let
                        answered =
                            updateAnswer r quizz.current

                        answeredQuestions =
                            answered :: quizz.pastQuestions
                    in
                    case quizz.nextQuestions of
                        [] ->
                            ( QuizzCompleted { answers = answeredQuestions }
                            , Cmd.none
                            )

                        q :: qs ->
                            ( QuizzInProgress
                                { quizz
                                    | pastQuestions = answeredQuestions
                                    , current = q
                                    , nextQuestions = qs
                                }
                            , Cmd.none
                            )

                NoOp ->
                    ( state
                    , Cmd.none
                    )
