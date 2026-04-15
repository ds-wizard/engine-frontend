module Common.Components.MarkdownEditor exposing
    ( labels
    , markdownEditor
    , mode
    , onChange
    , onFocus
    , onModeChange
    , value
    )

import Gettext exposing (gettext)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Json.Encode as E


markdownEditor : List (Html.Attribute msg) -> Html msg
markdownEditor attributes =
    Html.node "markdown-editor" attributes []


value : String -> Html.Attribute msg
value =
    Html.Attributes.property "editorValue" << E.string


mode : String -> Html.Attribute msg
mode =
    Html.Attributes.property "editorMode" << E.string


labels : Gettext.Locale -> Html.Attribute msg
labels locale =
    Html.Attributes.property "labels" <|
        E.object
            [ ( "heading2", E.string (gettext "Heading 2" locale) )
            , ( "heading3", E.string (gettext "Heading 3" locale) )
            , ( "bold", E.string (gettext "Bold" locale) )
            , ( "italic", E.string (gettext "Italic" locale) )
            , ( "strike", E.string (gettext "Strikethrough" locale) )
            , ( "link", E.string (gettext "Link" locale) )
            , ( "image", E.string (gettext "Image" locale) )
            , ( "bulletList", E.string (gettext "Bullet List" locale) )
            , ( "orderedList", E.string (gettext "Ordered List" locale) )
            , ( "code", E.string (gettext "Inline Code" locale) )
            , ( "codeBlock", E.string (gettext "Code Block" locale) )
            , ( "richText", E.string (gettext "Rich Text" locale) )
            , ( "markdown", E.string (gettext "Markdown" locale) )
            ]


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.on "editorChanged" <|
        D.map toMsg <|
            D.at [ "target", "editorValue" ] D.string


onModeChange : (String -> msg) -> Html.Attribute msg
onModeChange toMsg =
    Html.Events.on "editorModeChanged" <|
        D.map toMsg <|
            D.at [ "target", "editorMode" ] D.string


onFocus : msg -> Html.Attribute msg
onFocus toMsg =
    Html.Events.on "focus" <|
        D.succeed toMsg
