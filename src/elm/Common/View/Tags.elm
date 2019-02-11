module Common.View.Tags exposing (tagList)

import Common.View.Flash as Flash
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Tag)
import Utils exposing (getContrastColorHex)


type alias TagListConfig msg =
    { selected : List String
    , addMsg : String -> msg
    , removeMsg : String -> msg
    }


tagList : TagListConfig msg -> List Tag -> Html msg
tagList config tags =
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
