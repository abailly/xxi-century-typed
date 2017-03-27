module Quizz exposing (main)

import Html as H
import Html.Attributes exposing (type_, width, style)


-- * Types


type alias Quizz =
    { current : Question }


initialQuizz : Quizz
initialQuizz =
    Quizz (Question "What is your name?" "Sir Arthur" Nothing)


type alias Question =
    { question : String
    , expected : String
    , response : Maybe String
    }


type Msg
    = NoOp


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
    quizz ! []


view : Quizz -> H.Html Msg
view { current } =
    let
        answer =
            case current.response of
                Nothing ->
                    H.span [ style [ ( "color", "orange" ), ( "font-weight", "bold" ) ] ] [ H.text "?" ]

                Just r ->
                    if r == expected then
                        H.span [ style [ ( "color", "green" ), ( "font-weight", "bold" ) ] ] [ H.text "Yes" ]
                    else
                        H.span [ style [ ( "color", "red" ), ( "font-weight", "bold" ) ] ] [ H.text "No" ]
    in
        H.div []
            [ H.label [] [ H.text <| current.question ]
            , H.input [ type_ "text", width 20 ] []
            , answer
            ]
