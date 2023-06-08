module Wizard.Common.View.Tag exposing
    ( SelectionConfig
    , TagListConfig
    , list
    , readOnlyList
    , selection
    , viewList
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, classList, disabled, id, style, type_)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (emptyNode)
import Shared.Utils exposing (getContrastColorHex)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltipCustom)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra


type alias TagListConfig msg =
    { selected : List String
    , addMsg : String -> msg
    , removeMsg : String -> msg
    , showDescription : Bool
    }


list : AppState -> TagListConfig msg -> List Tag -> Html msg
list appState config tags =
    let
        content =
            if List.length tags > 0 then
                List.map (tagView appState config) (List.sortBy .name tags)

            else
                [ Flash.info appState <| gettext "There are no question tags configured for the Knowledge Model." appState.locale ]
    in
    div [ class "tag-list" ] content


tagView : AppState -> TagListConfig msg -> Tag -> Html msg
tagView appState config tag =
    let
        selected =
            List.member tag.uuid config.selected

        msg =
            if selected then
                config.removeMsg tag.uuid

            else
                config.addMsg tag.uuid

        ( untitled, tagName ) =
            if String.isEmpty tag.name then
                ( True, gettext "Untitled question tag" appState.locale )

            else
                ( False, tag.name )

        tooltipAttrs =
            case ( tag.description, config.showDescription ) of
                ( Just description, True ) ->
                    tooltipCustom "with-tooltip-wide" description

                _ ->
                    []
    in
    div (class "tag" :: tooltipAttrs)
        [ label
            [ class "tag-label"
            , style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            , dataCy "tag"
            , classList [ ( "untitled", untitled ) ]
            ]
            [ input
                [ type_ "checkbox"
                , checked selected
                , onClick msg
                ]
                []
            , text tagName
            ]
        ]


type alias SelectionConfig msg =
    { tagListConfig : TagListConfig msg
    , useAllQuestions : Bool
    , useAllQuestionsMsg : Bool -> msg
    }


selection : AppState -> SelectionConfig msg -> ActionResult KnowledgeModel -> Html msg
selection appState selectionConfig knowledgeModelResult =
    let
        viewContent content =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (gettext "Question Tags" appState.locale) ]
                , div []
                    [ content ]
                ]
    in
    case knowledgeModelResult of
        Unset ->
            emptyNode

        Loading ->
            viewContent <|
                Flash.loader appState

        Error err ->
            viewContent <|
                Flash.error appState err

        Success knowledgeModel ->
            let
                tags =
                    KnowledgeModel.getTags knowledgeModel
            in
            if List.length tags > 0 then
                viewContent <|
                    div [ class "tag-selection tag-selection-form" ]
                        [ FormExtra.text <| gettext "You can either use all questions from the knowledge model or filter them by question tags." appState.locale
                        , label [ onClick (selectionConfig.useAllQuestionsMsg True) ]
                            [ input
                                [ id "question-tags-all"
                                , type_ "radio"
                                , checked selectionConfig.useAllQuestions
                                ]
                                []
                            , text (gettext "Use all questions" appState.locale)
                            ]
                        , label [ onClick (selectionConfig.useAllQuestionsMsg False) ]
                            [ input
                                [ id "question-tags-filter"
                                , type_ "radio"
                                , checked (not selectionConfig.useAllQuestions)
                                ]
                                []
                            , text (gettext "Filter by question tags" appState.locale)
                            ]
                        , Html.viewIf (not selectionConfig.useAllQuestions) <|
                            list appState selectionConfig.tagListConfig tags
                        ]

            else
                viewContent <|
                    Flash.info appState <|
                        gettext "There are no question tags for this knowledge model." appState.locale


readOnlyList : AppState -> List String -> List Tag -> Html msg
readOnlyList appState selected tags =
    let
        content =
            if List.isEmpty tags then
                [ Flash.info appState (gettext "There are no question tags for this knowledge model." appState.locale) ]

            else if List.isEmpty selected then
                [ Flash.info appState (gettext "All questions are used." appState.locale) ]

            else
                List.map (readOnlyTagView selected) (List.sortBy .name tags)
    in
    div [ class "tag-list tag-list-readonly" ] content


readOnlyTagView : List String -> Tag -> Html msg
readOnlyTagView selected tag =
    let
        isSelected =
            List.member tag.uuid selected
    in
    div [ class "tag" ]
        [ label
            [ class "tag-label"
            , style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            ]
            [ input
                [ type_ "checkbox"
                , checked isSelected
                , disabled True
                ]
                []
            , text tag.name
            ]
        ]


viewList : List Tag -> Html msg
viewList tags =
    if List.isEmpty tags then
        emptyNode

    else
        div [ class "tag-list tag-list-view" ] (List.map viewListTagView tags)


viewListTagView : Tag -> Html msg
viewListTagView tag =
    div [ class "tag" ]
        [ label
            [ class "tag-label"
            , style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            ]
            [ text tag.name
            ]
        ]
