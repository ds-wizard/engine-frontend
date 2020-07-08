module Wizard.Public.BookReference.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Shared.Data.BookReference exposing (BookReference)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Public.BookReference.Models exposing (Model)
import Wizard.Public.BookReference.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.BookReference.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Public.BookReference.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewBookReference appState) model.bookReference


bookUrl : String
bookUrl =
    "https://www.crcpress.com/Data-Stewardship-for-Discovery-A-Practical-Guide-for-Data-Experts/Mons/p/book/9781498753173"


crcUrl : String
crcUrl =
    "https://taylorandfrancis.com"


viewBookReference : AppState -> BookReference -> Html Msg
viewBookReference appState bookReference =
    div [ class "Public__BookReference" ]
        [ div [ class "jumbotron" ]
            [ div [ class "book-name" ]
                [ a [ href bookUrl, target "_blank" ]
                    [ img [ src "/img/book-preview.png", alt "Book" ] []
                    , lx_ "bookName" appState
                    ]
                , text <| ": " ++ l_ "bookChapter" appState ++ " " ++ bookReference.bookChapter
                ]
            , div [ class "book-crc" ]
                [ div [] [ lx_ "permission" appState ]
                , a [ href crcUrl, target "_blank" ]
                    [ img [ src "/img/crc-logo.png", alt "CRC Press" ] []
                    ]
                ]
            ]
        , Markdown.toHtml [] bookReference.content
        ]
