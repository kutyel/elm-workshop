module Step15.Tests.Tests exposing (..)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import ElmEscapeHtml exposing (unescape)
import Expect
import Fuzz
import Http exposing (Error(..))
import Random
import Step15.Main exposing (..)
import Test exposing (Test, concat, fuzz)
import Test.Html.Event exposing (click, simulate, toResult)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import Test.Runner.Html exposing (defaultConfig, hidePassedTests, viewResults)
import Url exposing (Protocol(..), Url)
import Utils.Utils exposing (testStyles)


fakeHomeUrl : Url
fakeHomeUrl =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 80
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }


fakeCategoriesUrl : Url
fakeCategoriesUrl =
    { fakeHomeUrl | fragment = Just "categories" }


fakeResultUrl : Int -> Url
fakeResultUrl score =
    { fakeHomeUrl | fragment = Just ("result/" ++ String.fromInt score) }


fakeGameUrl : Url
fakeGameUrl =
    { fakeHomeUrl | fragment = Just "game" }


main : Program () Key ()
main =
    let
        testsView key =
            Document
                "Tests for step 14"
                [ testStyles
                , viewResults (Random.initialSeed 1000 |> defaultConfig |> hidePassedTests) (testsSuite key)
                ]

        init _ _ key =
            ( key, Cmd.none )

        update _ key =
            ( key, Cmd.none )
    in
    Browser.application
        { init = init
        , update = update
        , view = testsView
        , subscriptions = always Sub.none
        , onUrlRequest = always ()
        , onUrlChange = always ()
        }


categoriesUrl : String
categoriesUrl =
    "https://opentdb.com/api_category.php"


questionsUrl : String
questionsUrl =
    "https://opentdb.com/api.php?amount=5&type=multiple"


testsSuite : Key -> Test
testsSuite key =
    concat
        [ whenQuestionsAreLoadedTheFirstQuestionShouldBeDisplayed key
        , afterClickingTheProperAnswerTheModelShouldBeUpdated key
        , afterClickingTheWrongAnswerTheModelShouldBeUpdated key
        , afterAnsweringLastQuestionWeShouldBeRedirectedToResult key
        ]


whenQuestionsAreLoadedTheFirstQuestionShouldBeDisplayed : Key -> Test
whenQuestionsAreLoadedTheFirstQuestionShouldBeDisplayed key =
    fuzz randomTwoQuestionsListFuzz "When questions are loaded, the first question should be displayed" <|
        \randomQuestions ->
            case randomQuestions of
                [ question1, question2 ] ->
                    let
                        updatedView =
                            init () fakeHomeUrl key
                                |> Tuple.first
                                |> update (OnQuestionsFetched <| Ok [ question1, question2 ])
                                |> Tuple.first
                                |> view
                    in
                    updatedView
                        |> Query.fromHtml
                        |> Query.has [ text (unescape question1.question) ]

                _ ->
                    Expect.pass


afterClickingTheProperAnswerTheModelShouldBeUpdated : Key -> Test
afterClickingTheProperAnswerTheModelShouldBeUpdated key =
    fuzz randomTwoQuestionsListFuzz "After clicking the proper answer, model should indicate that it's correct and go to next question" <|
        \randomQuestions ->
            case randomQuestions of
                [ question1, question2 ] ->
                    let
                        initialModel =
                            init () fakeHomeUrl key
                                |> Tuple.first
                                |> update (OnQuestionsFetched <| Ok [ question1, question2 ])
                                |> Tuple.first

                        modelAfterClickOnProperAnswer =
                            view initialModel
                                |> Query.fromHtml
                                |> Query.findAll [ tag "a" ]
                                |> Query.first
                                |> simulate click
                                |> toResult
                                |> Result.map (\msg -> update msg initialModel)
                                |> Result.map Tuple.first

                        expectedGame =
                            Game [ AnsweredQuestion question1 Correct ] question2 []
                    in
                    case modelAfterClickOnProperAnswer of
                        Err _ ->
                            Expect.fail "A click on an answer should generate a message to update the model"

                        Ok model ->
                            Expect.equal (Model Loading <| GameRoute (Loaded expectedGame)) model

                _ ->
                    Expect.pass


afterClickingTheWrongAnswerTheModelShouldBeUpdated : Key -> Test
afterClickingTheWrongAnswerTheModelShouldBeUpdated key =
    fuzz randomTwoQuestionsListFuzz "After clicking the wrong answer, model should indicate that it's incorrect and go to next question" <|
        \randomQuestions ->
            case randomQuestions of
                [ question1, question2 ] ->
                    let
                        initialModel =
                            init () fakeHomeUrl key
                                |> Tuple.first
                                |> update (OnQuestionsFetched <| Ok [ question1, question2 ])
                                |> Tuple.first

                        modelAfterClickOnWrongAnswer =
                            view initialModel
                                |> Query.fromHtml
                                |> Query.findAll [ tag "a" ]
                                |> Query.index 1
                                |> simulate click
                                |> toResult
                                |> Result.map (\msg -> update msg initialModel)
                                |> Result.map Tuple.first

                        expectedGame =
                            Game [ AnsweredQuestion question1 Incorrect ] question2 []
                    in
                    case modelAfterClickOnWrongAnswer of
                        Err _ ->
                            Expect.fail "A click on an answer should generate a message to update the model"

                        Ok model ->
                            Expect.equal (Model Loading <| GameRoute (Loaded expectedGame)) model

                _ ->
                    Expect.pass


afterAnsweringLastQuestionWeShouldBeRedirectedToResult : Key -> Test
afterAnsweringLastQuestionWeShouldBeRedirectedToResult _ =
    fuzz randomTwoQuestionsListFuzz "After answering the last question, we should be redirect to result page" <|
        \randomQuestions ->
            case randomQuestions of
                [ question1, question2 ] ->
                    let
                        initialModel =
                            Game [ AnsweredQuestion question1 Correct ] question2 []
                                |> Loaded
                                |> GameRoute
                                |> Model Loading

                        modelAfterClickOnAnswer =
                            view initialModel
                                |> Query.fromHtml
                                |> Query.findAll [ tag "a" ]
                                |> Query.index 1
                                |> simulate click
                                |> toResult
                                |> Result.map (\msg -> update msg initialModel)
                                |> Result.map Tuple.first
                    in
                    case modelAfterClickOnAnswer of
                        Err _ ->
                            Expect.fail "A click on an answer should generate a message to update the model"

                        Ok { route } ->
                            case route of
                                ResultRoute _ ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "The user should be redirected to the score page"

                _ ->
                    Expect.pass


afterAnsweringLastQuestionWeShouldBeRedirectedToProperResult : Test
afterAnsweringLastQuestionWeShouldBeRedirectedToProperResult =
    fuzz randomTwoQuestionsListFuzz "After answering the last question, we should be redirect to result page with the proper score" <|
        \randomQuestions ->
            case randomQuestions of
                [ question1, question2 ] ->
                    let
                        initialModel =
                            Game [ AnsweredQuestion question1 Correct ] question2 []
                                |> Loaded
                                |> GameRoute
                                |> Model Loading

                        modelAfterClickOnWrongAnswer =
                            view initialModel
                                |> Query.fromHtml
                                |> Query.findAll [ tag "a" ]
                                |> Query.index 1
                                |> simulate click
                                |> toResult
                                |> Result.map (\msg -> update msg initialModel)
                                |> Result.map Tuple.first
                    in
                    case modelAfterClickOnWrongAnswer of
                        Err _ ->
                            Expect.fail "A click on an answer should generate a message to update the model"

                        Ok { route } ->
                            case route of
                                ResultRoute 1 ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "The user should be redirected to the score page with score 1"

                _ ->
                    Expect.pass


randomTwoQuestionsListFuzz : Fuzz.Fuzzer (List Question)
randomTwoQuestionsListFuzz =
    Fuzz.map2
        (List.singleton >> (\b a -> a :: b))
        randomQuestionFuzz
        randomQuestionFuzz


randomQuestionFuzz : Fuzz.Fuzzer Question
randomQuestionFuzz =
    Fuzz.map5
        (\question answer1 answer2 answer3 answer4 ->
            Question question answer1 [ answer1, answer2, answer3, answer4 ]
        )
        Fuzz.string
        Fuzz.string
        Fuzz.string
        Fuzz.string
        Fuzz.string
