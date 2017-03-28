module Quizz exposing (main, ResponseStatus(..), checkResponseVsExpectation, qcm, question, updateAnswer, Question)

import Html as H
import Html.Attributes exposing (type_, width, style, value, selected)
import Html.Events exposing (on, keyCode, onInput, targetValue)
import Json.Decode as Json


-- * Types


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


{-| Main
-}
main : Program Never Model Msg
main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    initialModel ! []


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
                    Debug.log (toString r) <|
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


view : Model -> H.Html Msg
view state =
    case state of
        QuizzInProgress quizz ->
            H.div []
                [ viewPastQuestions quizz.pastQuestions
                , viewCurrentQuestion quizz
                ]

        QuizzCompleted quizz ->
            H.div []
                [ viewPastQuestions quizz.answers
                ]


viewPastQuestions : List Question -> H.Html Msg
viewPastQuestions questions =
    H.div [] <| List.map viewPastQuestion <| List.reverse questions


viewPastQuestion : Question -> H.Html Msg
viewPastQuestion question =
    let
        styles =
            case checkResponseVsExpectation question of
                Unknown ->
                    [ ( "color", "orange" ), ( "font-weight", "bold" ) ]

                Correct ->
                    [ ( "color", "green" ), ( "font-weight", "bold" ) ]

                Incorrect ->
                    [ ( "color", "red" ), ( "font-weight", "bold" ) ]
    in
        H.div []
            [ H.div [] [ H.text <| showQuestion question ]
            , H.div [ style styles ] [ H.text <| showResponse question ]
            ]


viewCurrentQuestion : Quizz -> H.Html Msg
viewCurrentQuestion { pastQuestions, current, nextQuestions, currentResponse } =
    H.div []
        [ H.label [] [ H.text <| showQuestion current ]
        , viewQuestion currentResponse current
        ]


viewQuestion : String -> Question -> H.Html Msg
viewQuestion currentResponse question =
    case question of
        Question q ->
            viewOpenQuestion currentResponse q

        QCM q ->
            viewQCM currentResponse q


viewOpenQuestion : String -> OpenQuestion -> H.Html Msg
viewOpenQuestion currentResponse _ =
    H.input
        [ type_ "text"
        , width 20
        , onInput UpdateResponse
        , onEnter (SubmitResponse <| Just currentResponse)
        , value currentResponse
        ]
        []


viewQCM : String -> QCMQuestion -> H.Html Msg
viewQCM currentResponse { options } =
    let
        viewOption sel opt =
            H.option [ value opt ] [ H.text opt ]

        viewOptions =
            H.option [ value "" ] [ H.text "Choose" ] :: List.map (viewOption currentResponse) options
    in
        H.select
            [ onSelect (Just >> SubmitResponse) ]
            viewOptions


onSelect : (String -> msg) -> H.Attribute msg
onSelect msg =
    on "change" (Json.map msg targetValue)


onEnter : msg -> H.Attribute msg
onEnter msg =
    on "keydown"
        (keyCode
            |> Json.andThen is13
            |> Json.andThen (always <| Json.succeed msg)
        )


is13 : Int -> Json.Decoder ()
is13 code =
    if code == 13 then
        Json.succeed ()
    else
        Json.fail "not the right key code"
