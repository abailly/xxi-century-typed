module Quizz exposing (main, ResponseStatus(..), checkResponseVsExpectation, Question)

import Html as H
import Html.Attributes exposing (type_, width, style, value)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json


-- * Types


type alias Quizz =
    { current : Question
    , currentResponse :
        String
        -- UI only state
    }


initialQuizz : Quizz
initialQuizz =
    Quizz (Question "What is your name?" "Sir Arthur" Nothing) ""


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
    = CheckResponse (Maybe String)
    | UpdateResponse String
    | NoOp


{-| Main
-}
main : Program Never Quizz Msg
main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Quizz, Cmd Msg )
init =
    initialQuizz ! []


update : Msg -> Quizz -> ( Quizz, Cmd Msg )
update msg quizz =
    case msg of
        UpdateResponse s ->
            { quizz | currentResponse = s } ! []

        CheckResponse r ->
            let
                q =
                    quizz.current
            in
                { quizz | current = { q | response = r } } ! []

        NoOp ->
            quizz ! []


view : Quizz -> H.Html Msg
view { current, currentResponse } =
    let
        answer =
            case checkResponseVsExpectation current of
                Unknown ->
                    H.span [ style [ ( "color", "orange" ), ( "font-weight", "bold" ) ] ] [ H.text "?" ]

                Correct ->
                    H.span [ style [ ( "color", "green" ), ( "font-weight", "bold" ) ] ] [ H.text "Yes" ]

                Incorrect ->
                    H.span [ style [ ( "color", "red" ), ( "font-weight", "bold" ) ] ] [ H.text "No" ]
    in
        H.div []
            [ H.label [] [ H.text <| current.question ]
            , H.input
                [ type_ "text"
                , width 20
                , onInput UpdateResponse
                , onEnter (CheckResponse <| Just currentResponse)
                , value currentResponse
                ]
                []
            , answer
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
