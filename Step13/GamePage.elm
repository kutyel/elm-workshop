module Step13.GamePage exposing
    ( Category
    , Game
    , Model
    , Msg(..)
    , Question
    , RemoteData(..)
    , displayAnswer
    , displayTestsAndView
    , gamePage
    , init
    , main
    , update
    , view
    )

import Browser
import Html exposing (Html, a, div, h2, li, text, ul)
import Html.Attributes exposing (class)
import Http exposing (expectJson)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Utils.Utils exposing (styles, testsIframe)


questionsUrl : String
questionsUrl =
    "https://opentdb.com/api.php?amount=5&type=multiple"


loadQuestions : Cmd Msg
loadQuestions =
    Http.get
        { url = questionsUrl
        , expect = expectJson OnQuestionsFetched resultsDecoder
        }


resultsDecoder : Decode.Decoder (List Question)
resultsDecoder =
    Decode.field "results" quetionsDecoder


quetionsDecoder : Decode.Decoder (List Question)
quetionsDecoder =
    let
        question =
            Decode.field "question" Decode.string

        correctAnswer =
            Decode.field "correct_answer" Decode.string

        answers =
            Decode.succeed (::)
                |> Decode.andMap correctAnswer
                |> Decode.andMap (Decode.field "incorrect_answers" (Decode.list Decode.string))
    in
    Decode.list
        (Decode.succeed Question
            |> Decode.andMap question
            |> Decode.andMap correctAnswer
            |> Decode.andMap answers
        )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = displayTestsAndView
        , subscriptions = always Sub.none
        }


type alias Question =
    { question : String
    , correctAnswer : String
    , answers : List String
    }


type alias Model =
    { game : RemoteData Game
    }


type alias Game =
    { currentQuestion : Question
    , remainingQuestions : List Question
    }


type Msg
    = OnQuestionsFetched (Result Http.Error (List Question))


type alias Category =
    { id : Int
    , name : String
    }


type RemoteData a
    = Loading
    | Loaded a
    | OnError


init : ( Model, Cmd Msg )
init =
    ( Model Loading, loadQuestions )


update : Msg -> Model -> ( Model, Cmd Msg )
update message _ =
    case message of
        OnQuestionsFetched (Ok (currentQuestion :: restOfQuestions)) ->
            ( { game =
                    Loaded
                        { currentQuestion = currentQuestion
                        , remainingQuestions = restOfQuestions
                        }
              }
            , Cmd.none
            )

        OnQuestionsFetched _ ->
            ( { game = OnError }, Cmd.none )


view : Model -> Html.Html Msg
view { game } =
    case game of
        Loading ->
            div [] [ text "Loading the questions..." ]

        Loaded { currentQuestion } ->
            div [] [ gamePage currentQuestion ]

        OnError ->
            div [] [ text "An unknown error occurred while loading the questions" ]


gamePage : Question -> Html msg
gamePage question =
    div []
        [ h2 [ class "question" ] [ text question.question ]
        , ul [ class "answers" ] (List.map displayAnswer question.answers)
        ]


displayAnswer : String -> Html msg
displayAnswer answer =
    li [] [ a [ class "btn btn-primary" ] [ text answer ] ]



------------------------------------------------------------------------------------------------------
-- Don't modify the code below, it displays the view and the tests and helps with testing your code --
------------------------------------------------------------------------------------------------------


displayTestsAndView : Model -> Html Msg
displayTestsAndView model =
    div []
        [ styles
        , div [ class "jumbotron" ] [ view model ]
        , testsIframe
        ]
