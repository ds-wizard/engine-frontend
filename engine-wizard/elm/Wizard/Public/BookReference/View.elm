module Wizard.Public.BookReference.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (alt, class, href, src, target)
import Shared.Data.BookReference exposing (BookReference)
import Shared.Markdown as Markdown
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Public.BookReference.Models exposing (Model)
import Wizard.Public.BookReference.Msgs exposing (Msg)


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
        [ div [ class "px-4 py-5 bg-light rounded-3 book-title" ]
            [ div [ class "book-name" ]
                [ a [ href bookUrl, target "_blank" ]
                    [ img [ src "/wizard/img/book-preview.png", alt "Data Stewardship for Open Science Book Cover" ] []
                    , text (gettext "Data Stewardship for Open Science" appState.locale)
                    ]
                , text <| ": " ++ String.format (gettext "Chapter %s" appState.locale) [ bookReference.bookChapter ]
                ]
            , div [ class "book-crc" ]
                [ div [] [ text (gettext "With kind permission of" appState.locale) ]
                , a [ href crcUrl, target "_blank" ]
                    [ img [ src "/wizard/img/crc-logo.png", alt "CRC Press" ] []
                    ]
                ]
            ]
        , Markdown.toHtml [] bookReference.content
        ]
