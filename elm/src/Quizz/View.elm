module Quizz.View exposing (view)

import Html as H
import Html.Attributes exposing (type_, width, style, value, selected)
import Html.Events exposing (on, keyCode, onInput, targetValue)
import Json.Decode as Json
import Quizz.Types exposing (..)


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
