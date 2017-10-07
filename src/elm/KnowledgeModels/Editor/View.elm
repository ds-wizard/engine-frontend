module KnowledgeModels.Editor.View exposing (..)

import Common.Html exposing (pageHeader)
import Html exposing (Html, a, button, div, i, input, label, li, option, select, text, textarea, ul)
import Html.Attributes exposing (class, href, rows, type_)
import Msgs exposing (Msg)


view : Html Msg
view =
    div []
        [ pageHeader "Knowledge Model Editor" []
        , breadcrumbs [ "My Knowledge Model", "Chapter 1", "Question 3" ]
        , inputText "Name"
        , inputTextarea "Text"
        , inputSelect "Type" [ "Answers", "Text" ]
        , inputChildren "Answers" [ "Skip", "Explore" ]
        , formActions
        ]



-- Breadcrumbs


breadcrumbs : List String -> Html Msg
breadcrumbs elements =
    ul [ class "breadcrumb" ]
        (List.map breadcrumbsElement elements)


breadcrumbsElement : String -> Html Msg
breadcrumbsElement name =
    li [] [ text name ]



-- Inputs


inputText : String -> Html Msg
inputText labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , input [ class "form-control", type_ "text" ] []
        ]


inputTextarea : String -> Html Msg
inputTextarea labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , textarea [ class "form-control", rows 4 ] []
        ]


inputSelect : String -> List String -> Html Msg
inputSelect labelText options =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , select [ class "form-control" ] (List.map inputSelectOption options)
        ]


inputSelectOption : String -> Html Msg
inputSelectOption labelText =
    option [] [ text labelText ]



-- Children Input


inputChildren : String -> List String -> Html Msg
inputChildren labelText children =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , div [] (List.map inputChildrenChild children)
        , a [ class "link-with-icon", href "#" ]
            [ i [ class "fa fa-plus" ] []
            , text "Add answer"
            ]
        ]


inputChildrenChild : String -> Html Msg
inputChildrenChild name =
    div [ class "panel panel-default panel-input-child" ]
        [ div [] [ text name ]
        , div [ class "panel-input-child-actions" ]
            [ a [ href "#" ] [ text "Edit" ]
            , a [ href "#" ] [ text "Delete" ]
            ]
        ]



-- Form Actions


formActions : Html Msg
formActions =
    div [ class "form-actions" ]
        [ button [ class "btn btn-default" ] [ text "Cancel" ]
        , button [ class "btn btn-primary" ] [ text "Save" ]
        ]
