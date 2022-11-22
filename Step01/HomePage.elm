module Step01.HomePage exposing (homePage, main)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Utils.Utils exposing (styles, testsIframe)


homePage : Html msg
homePage =
    div [ class "gameOptions" ]
        [ h1 [] [ text "Quiz Game" ]
        , a [ class "btn btn-primary", href "#game" ] [ text "Play random questions" ]
        , a [ class "btn btn-primary", href "#categories" ] [ text "Play from a category" ]
        ]



------------------------------------------------------------------------------------------------------------------------
-- You don't need to worry about the code below, it only displays the result of your code and the result of the tests --
------------------------------------------------------------------------------------------------------------------------


main =
    div []
        [ styles
        , div [ class "jumbotron" ] [ homePage ]
        , testsIframe
        ]
