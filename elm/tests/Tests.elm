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
                assertResponseIs (Question "Foo?" "bar" Nothing) Unknown
            , test "yields Correct given response matches expected" <|
                assertResponseIs (Question "Foo?" "bar" (Just "bar")) Correct
            , test "yields Incorrect given response does not match expected" <|
                assertResponseIs (Question "Foo?" "bar" (Just "baz")) Incorrect
            ]
        ]
