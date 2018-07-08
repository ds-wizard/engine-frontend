module Public.BookReference.View exposing (..)

import Common.View exposing (fullPageActionResultView)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Msgs
import Public.BookReference.Models exposing (BookReference, Model)


view : Model -> Html Msgs.Msg
view model =
    fullPageActionResultView viewBookReference model.bookReference


bookUrl : String
bookUrl =
    "https://www.crcpress.com/Data-Stewardship-for-Discovery-A-Practical-Guide-for-Data-Experts/Mons/p/book/9781498753173"


crcUrl : String
crcUrl =
    "https://taylorandfrancis.com"


viewBookReference : BookReference -> Html Msgs.Msg
viewBookReference bookReference =
    div [ class "Public__BookReference" ]
        [ div [ class "jumbotron" ]
            [ div [ class "book-name" ]
                [ i [ class "fa fa-book" ] []
                , a [ href bookUrl, target "_blank" ] [ text "Data Stewardship for Open Science" ]
                , text <| ": Chapter " ++ bookReference.bookChapter
                ]
            , div [ class "book-crc" ]
                [ div [] [ text "With kind permission of" ]
                , a [ href crcUrl, target "_blank" ]
                    [ img [ src "/img/crc-logo.png", alt "CRC Press" ] []
                    ]
                ]
            ]
        , Markdown.toHtml [] bookReference.content
        ]
