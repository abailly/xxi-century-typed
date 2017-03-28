module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Quizz exposing (..)


assertResponseIs : Question -> ResponseStatus -> (() -> Expect.Expectation)
assertResponseIs question expectedStatus =
    \() ->
        let
            actual =
                checkResponseVsExpectation question
        in
            Expect.equal actual expectedStatus


all : Test
all =
    describe "Holy Grail Test Suite"
        [ describe "Checking Answer"
            [ test "yields Unknown given no response" <|
                assertResponseIs (updateAnswer Nothing <| question "Foo?" "bar") Unknown
            , test "yields Correct given response matches expected" <|
                assertResponseIs (updateAnswer (Just "bar") <| question "Foo?" "bar") Correct
            , test "yields Incorrect given response does not match expected" <|
                assertResponseIs (updateAnswer (Just "baz") <| question "Foo?" "bar") Incorrect
            ]
        , describe "Updating QCM with Answer"
            [ test "updates question given update is a valid integer" <|
                assertResponseIs (updateAnswer (Just "bar") <| qcm "Foo?" [ "bar", "baz" ] "bar") Correct
            ]
        ]
