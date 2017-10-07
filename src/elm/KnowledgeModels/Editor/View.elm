module KnowledgeModels.Editor.View exposing (..)

import Common.Html exposing (pageHeader)
import Html exposing (Html, a, button, div, h3, i, input, label, li, option, select, text, textarea, ul)
import Html.Attributes exposing (class, href, rows, type_)
import Msgs exposing (Msg)
import String exposing (toLower)


view : Html Msg
view =
    viewKnowledgeModel


viewKnowledgeModel : Html Msg
viewKnowledgeModel =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model" ]
        , editorTitle "Knowledge Model"
        , inputText "Name"
        , inputChildren "Chapter" [ "Design of experiment", "Data design and planning" ]
        , formActions
        ]


viewChapter : Html Msg
viewChapter =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model", "Chapter 1" ]
        , editorTitle "Chapter"
        , inputText "Title"
        , inputTextarea "Text"
        , inputChildren "Question" [ "Will you be using new types of data?", "How will you be storin metadata?" ]
        , formActions
        ]


viewQuestion : Html Msg
viewQuestion =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model", "Chapter 1", "Question 1" ]
        , editorTitle "Question"
        , inputText "Title"
        , inputSelect "Type" [ "Answer", "Text" ]
        , inputTextarea "Text"
        , inputChildren "Answer" [ "Skip", "Explore" ]
        , inputChildren "Reference" [ "Reference 1", "Reference 2" ]
        , inputChildren "Expert" [ "Expert 1", "Expert 2" ]
        , formActions
        ]


viewAnswer : Html Msg
viewAnswer =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model", "Chapter 1", "Question 1", "Answer 1" ]
        , editorTitle "Answer"
        , inputText "Label"
        , inputTextarea "Advice"
        , inputChildren "Follow-up question" [ "Did you consider how to monitor data integrity?", "Do all datasets you work with have a license?" ]
        , formActions
        ]


viewExpert : Html Msg
viewExpert =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model", "Chapter 1", "Question 1", "Expert 1" ]
        , editorTitle "Expert"
        , inputText "Name"
        , inputText "Email"
        , formActions
        ]


viewReference : Html Msg
viewReference =
    div [ class editorClass ]
        [ editorHeader
        , breadcrumbs [ "My Knowledge Model", "Chapter 1", "Question 1", "Reference 1" ]
        , editorTitle "Reference"
        , inputText "Chapter"
        , formActions
        ]



-- Common


editorHeader : Html Msg
editorHeader =
    pageHeader "Knowledge Model Editor" []


editorTitle : String -> Html Msg
editorTitle title =
    h3 [] [ text title ]


editorClass : String
editorClass =
    "knowledge-model-editor col-xs-12 col-lg-10 col-lg-offset-1"



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
inputChildren childName children =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text (childName ++ "s") ]
        , div [] (List.map inputChildrenChild children)
        , a [ class "link-with-icon", href "#" ]
            [ i [ class "fa fa-plus" ] []
            , text ("Add " ++ toLower childName)
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
