module Quizz.Types exposing (Model(..), Msg(..), OpenQuestion, QCMQuestion, Question(..), Quizz, ResponseStatus(..), checkResponseVsExpectation, initialModel, mkQuestion, qcm, showQuestion, showResponse, updateAnswer)


type Model
    = QuizzInProgress Quizz
    | QuizzCompleted { answers : List Question }


type alias Quizz =
    { pastQuestions : List Question
    , current : Question
    , nextQuestions : List Question
    , currentResponse :
        String

    -- UI only state
    }


type Question
    = Question OpenQuestion
    | QCM QCMQuestion


type alias OpenQuestion =
    { question : String
    , expected : String
    , response : Maybe String
    }


type alias QCMQuestion =
    { question : String
    , options : List String
    , expected : String
    , response : Maybe String
    }


initialModel : Model
initialModel =
    QuizzInProgress
        { pastQuestions = []
        , current = mkQuestion "What is your name?" "Sir Arthur"
        , nextQuestions =
            [ mkQuestion "What is your quest?" "To seek the Holy Grail"
            , qcm "What is your favorite colour?" [ "blue", "green", "yellow", "don't know" ] "blue"
            ]
        , currentResponse = ""
        }


showQuestion : Question -> String
showQuestion question =
    case question of
        Question q ->
            q.question

        QCM q ->
            q.question


showResponse : Question -> String
showResponse question =
    Maybe.withDefault "" <|
        case question of
            Question q ->
                q.response

            QCM q ->
                q.response


mkQuestion : String -> String -> Question
mkQuestion q e =
    Question { question = q, expected = e, response = Nothing }


qcm : String -> List String -> String -> Question
qcm q o e =
    QCM { question = q, options = o, expected = e, response = Nothing }


updateAnswer : Maybe String -> Question -> Question
updateAnswer answer question =
    case question of
        Question open ->
            Question { open | response = answer }

        QCM aQcm ->
            QCM { aQcm | response = answer }


type ResponseStatus
    = Unknown
    | Correct
    | Incorrect


checkResponseVsExpectation : Question -> ResponseStatus
checkResponseVsExpectation question =
    case question of
        Question { expected, response } ->
            case response of
                Nothing ->
                    Unknown

                Just r ->
                    if r == expected then
                        Correct

                    else
                        Incorrect

        QCM { expected, response } ->
            case response of
                Nothing ->
                    Unknown

                Just i ->
                    if i == expected then
                        Correct

                    else
                        Incorrect


type Msg
    = SubmitResponse (Maybe String)
    | UpdateResponse String
    | NoOp
