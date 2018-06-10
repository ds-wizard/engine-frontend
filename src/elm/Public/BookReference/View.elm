module Public.BookReference.View exposing (..)

import Common.View exposing (fullPageActionResultView)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Markdown
import Msgs
import Public.BookReference.Models exposing (BookReference, Model)


view : Model -> Html Msgs.Msg
view model =
    fullPageActionResultView viewBookReference model.bookReference


viewBookReference : BookReference -> Html Msgs.Msg
viewBookReference bookReference =
    div [ class "Public__BookReference" ]
        [ Markdown.toHtml [] bookReference.content
        ]
