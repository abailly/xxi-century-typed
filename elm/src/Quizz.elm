module Quizz exposing (main, ResponseStatus(..), checkResponseVsExpectation, Question)

import Html as H
import Html.Attributes exposing (type_, width, style, value)
import Html.Events exposing (on, keyCode, onInput)
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
        , current = Question "What is your name?" "Sir Arthur" Nothing
        , nextQuestions = [ Question "What is your quest?" "To seek the Holy Grail" Nothing ]
        , currentResponse = ""
        }


type alias Question =
    { question : String
    , expected : String
    , response : Maybe String
    }


type ResponseStatus
    = Unknown
    | Correct
    | Incorrect


checkResponseVsExpectation : Question -> ResponseStatus
checkResponseVsExpectation { expected, response } =
    case response of
        Nothing ->
            Unknown

        Just r ->
            if r == expected then
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
                    let
                        answered =
                            (\q -> { q | response = r }) quizz.current

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
            [ H.div [] [ H.text <| question.question ]
            , H.div [ style styles ] [ H.text <| Maybe.withDefault "" question.response ]
            ]


viewCurrentQuestion : Quizz -> H.Html Msg
viewCurrentQuestion { pastQuestions, current, nextQuestions, currentResponse } =
    H.div []
        [ H.label [] [ H.text <| current.question ]
        , H.input
            [ type_ "text"
            , width 20
            , onInput UpdateResponse
            , onEnter (SubmitResponse <| Just currentResponse)
            , value currentResponse
            ]
            []
        ]


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
