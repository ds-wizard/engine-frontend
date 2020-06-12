module Wizard.Common.View.Tag exposing (TagListConfig, list, readOnlyList, selection)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, i, input, label, text)
import Html.Attributes exposing (checked, class, disabled, style, type_)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import Wizard.Utils exposing (getContrastColorHex)


type alias TagListConfig msg =
    { selected : List String
    , addMsg : String -> msg
    , removeMsg : String -> msg
    }


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.Tag"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.Tag"


list : AppState -> TagListConfig msg -> List Tag -> Html msg
list appState config tags =
    let
        content =
            if List.length tags > 0 then
                List.map (tagView config) (List.sortBy .name tags)

            else
                [ Flash.info appState <| l_ "list.empty" appState ]
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
        viewContent content =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text "Tags" ]
                , div []
                    [ content ]
                ]
    in
    case knowledgeModelResult of
        Unset ->
            emptyNode

        Loading ->
            emptyNode

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
                    div []
                        [ list appState tagListConfig tags
                        , FormExtra.text <| l_ "selection.info" appState
                        ]

            else
                emptyNode


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
