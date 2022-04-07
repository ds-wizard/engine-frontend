module Wizard.Common.View.Tag exposing (TagListConfig, list, readOnlyList, selection, viewList)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, i, input, label, text)
import Html.Attributes exposing (checked, class, classList, disabled, style, type_)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Shared.Utils exposing (getContrastColorHex)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra


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
                List.map (tagView appState config) (List.sortBy .name tags)

            else
                [ Flash.info appState <| l_ "list.empty" appState ]
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
                ( True, lg "tag.untitled" appState )

            else
                ( False, tag.name )
    in
    div [ class "tag" ]
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


selection : AppState -> TagListConfig msg -> ActionResult KnowledgeModel -> Html msg
selection appState tagListConfig knowledgeModelResult =
    let
        viewContent content =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (lg "tags" appState) ]
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
                    div []
                        [ list appState tagListConfig tags
                        , FormExtra.text <| l_ "selection.info" appState
                        ]

            else
                viewContent <|
                    Flash.info appState <|
                        l_ "selection.empty" appState


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
