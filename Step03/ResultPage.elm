module Step03.ResultPage exposing (main, resultPage)

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Utils.Utils exposing (styles, testsIframe)


resultPage : Int -> Html msg
resultPage score =
    div [ class "score" ]
        [ h1 [] [ text ("Your score: " ++ String.fromInt score ++ " / 5") ]
        , a [ class "btn btn-primary", href "#" ] [ text "Replay" ]
        , p []
            [ text <|
                if score < 4 then
                    "Keep going, I'm sure you can do better!"

                else
                    "Congrats, this is really good!"
            ]
        ]



------------------------------------------------------------------------------------------------------------------------
-- You don't need to worry about the code below, it only displays the result of your code and the result of the tests --
------------------------------------------------------------------------------------------------------------------------


main =
    div []
        [ styles
        , div [ class "jumbotron" ] [ resultPage 3 ]
        , testsIframe
        ]
