module KMEditor.Editor2.View exposing (view)

import Common.Html exposing (fa)
import Html exposing (..)
import Html.Attributes exposing (class)
import KMEditor.Editor2.Models exposing (Model)
import KMEditor.Editor2.Msgs exposing (Msg)
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "KMEditor__Editor2" ]
        [ editorHeader
        , text "Editor 2"
        ]


editorHeader : Html Msgs.Msg
editorHeader =
    div [ class "editor-header" ]
        [ div [ class "undo" ]
            [ a [] [ fa "undo" ]
            , a [ class "disabled" ] [ fa "repeat" ]
            ]
        , ul [ class "nav" ]
            [ a [ class "nav-link active" ]
                [ fa "sitemap", text "Knowledge Model" ]
            , a [ class "nav-link" ]
                [ fa "tags", text "Tags" ]
            , a [ class "nav-link" ]
                [ fa "eye", text "Preview" ]
            , a [ class "nav-link" ]
                [ fa "history", text "History" ]
            ]
        , div [ class "actions" ]
            [ button [ class "btn btn-primary btn-with-loader" ]
                [ text "Save" ]
            ]
        ]
