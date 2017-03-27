module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Quizz exposing (..)


all : Test
all =
    describe "Holy Grail Test Suite"
        [ describe "Checking Answer"
            [ test "yields Unknown given no response" <|
                \() ->
                    let
                        actual =
                            checkResponseVsExpectation (Question "Foo?" "bar" Nothing)
                    in
                        Expect.equal actual Unknown
            , test "yields Correct given response matches expected" <|
                \() ->
                    let
                        actual =
                            checkResponseVsExpectation (Question "Foo?" "bar" (Just "bar"))
                    in
                        Expect.equal actual Correct
            , test "yields Incorrect given response does not match expected" <|
                \() ->
                    let
                        actual =
                            checkResponseVsExpectation (Question "Foo?" "bar" (Just "baz"))
                    in
                        Expect.equal actual Incorrect
            ]
        ]
