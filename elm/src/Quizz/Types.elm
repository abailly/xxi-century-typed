module Quizz.Types exposing (..)


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


initialModel : Model
initialModel =
    QuizzInProgress
        { pastQuestions = []
        , current = question "What is your name?" "Sir Arthur"
        , nextQuestions =
            [ question "What is your quest?" "To seek the Holy Grail"
            , qcm "What is your favorite colour?" [ "blue", "green", "yellow", "don't know" ] "blue"
            ]
        , currentResponse = ""
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


question : String -> String -> Question
question q e =
    Question { question = q, expected = e, response = Nothing }


qcm : String -> List String -> String -> Question
qcm q o e =
    QCM { question = q, options = o, expected = e, response = Nothing }


updateAnswer : Maybe String -> Question -> Question
updateAnswer answer question =
    case question of
        Question open ->
            Question { open | response = answer }

        QCM qcm ->
            QCM { qcm | response = answer }


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
