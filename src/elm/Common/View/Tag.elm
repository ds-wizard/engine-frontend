module Common.View.Tag exposing (TagListConfig, list, readOnlyList, selection)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import Common.View.Flash as Flash
import Common.View.FormExtra as FormExtra
import Html exposing (Html, div, i, input, label, text)
import Html.Attributes exposing (checked, class, disabled, style, type_)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import Utils exposing (getContrastColorHex)


type alias TagListConfig msg =
    { selected : List String
    , addMsg : String -> msg
    , removeMsg : String -> msg
    }


list : TagListConfig msg -> List Tag -> Html msg
list config tags =
    let
        content =
            if List.length tags > 0 then
                List.map (tagView config) (List.sortBy .name tags)

            else
                [ Flash.info "There are no tags configured for the Knowledge Model" ]
    in
    div [ class "tag-list" ] content


tagView : TagListConfig msg -> Tag -> Html msg
tagView config tag =
    let
        selected =
            List.member tag.uuid config.selected

        msg =
            if selected then
                config.removeMsg tag.uuid

            else
                config.addMsg tag.uuid
    in
    div [ class "tag" ]
        [ label
            [ class "tag-label"
            , style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            ]
            [ input
                [ type_ "checkbox"
                , checked selected
                , onClick msg
                ]
                []
            , text tag.name
            ]
        ]


selection : TagListConfig msg -> ActionResult KnowledgeModel -> Html msg
selection tagListConfig knowledgeModelResult =
    let
        tagsContent =
            case knowledgeModelResult of
                Unset ->
                    div [ class "alert alert-light" ]
                        [ i [] [ text "Select the knowledge model first" ] ]

                Loading ->
                    Flash.loader

                Error err ->
                    Flash.error err

                Success knowledgeModel ->
                    let
                        tags =
                            KnowledgeModel.getTags knowledgeModel

                        extraText =
                            if List.length tags > 0 then
                                FormExtra.text "You can filter questions in the questionnaire by tags. If no tags are selected, all questions will be used."

                            else
                                emptyNode
                    in
                    div []
                        [ list tagListConfig tags
                        , extraText
                        ]
    in
    div [ class "form-group form-group-tags" ]
        [ label [] [ text "Tags" ]
        , div [] [ tagsContent ]
        ]


readOnlyList : List String -> List Tag -> Html msg
readOnlyList selected tags =
    let
        content =
            if List.length tags > 0 then
                List.map (readOnlyTagView selected) (List.sortBy .name tags)

            else
                [ div [ class "alert alert-light" ]
                    [ i [] [ text "No tags" ] ]
                ]
    in
    div [ class "tag-list" ] content


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
