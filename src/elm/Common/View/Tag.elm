module Common.View.Tag exposing (TagListConfig, list, readOnlyList, selection)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Locale exposing (l, lx)
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


l_ : String -> AppState -> String
l_ =
    l "Common.View.Tag"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Common.View.Tag"


list : AppState -> TagListConfig msg -> List Tag -> Html msg
list appState config tags =
    let
        content =
            if List.length tags > 0 then
                List.map (tagView config) (List.sortBy .name tags)

            else
                [ Flash.info <| l_ "list.empty" appState ]
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


selection : AppState -> TagListConfig msg -> ActionResult KnowledgeModel -> Html msg
selection appState tagListConfig knowledgeModelResult =
    let
        tagsContent =
            case knowledgeModelResult of
                Unset ->
                    div [ class "alert alert-light" ]
                        [ i []
                            [ lx_ "selection.notSelected" appState ]
                        ]

                Loading ->
                    Flash.loader appState

                Error err ->
                    Flash.error err

                Success knowledgeModel ->
                    let
                        tags =
                            KnowledgeModel.getTags knowledgeModel

                        extraText =
                            if List.length tags > 0 then
                                FormExtra.text <| l_ "selection.info" appState

                            else
                                emptyNode
                    in
                    div []
                        [ list appState tagListConfig tags
                        , extraText
                        ]
    in
    div [ class "form-group form-group-tags" ]
        [ label [] [ text "Tags" ]
        , div [] [ tagsContent ]
        ]


readOnlyList : AppState -> List String -> List Tag -> Html msg
readOnlyList appState selected tags =
    let
        content =
            if List.length tags > 0 then
                List.map (readOnlyTagView selected) (List.sortBy .name tags)

            else
                [ div [ class "alert alert-light" ]
                    [ i [] [ lx_ "readOnlyList.noTags" appState ] ]
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
