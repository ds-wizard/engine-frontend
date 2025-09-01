module Wizard.Components.CodeEditor exposing (chooseLanguage, codeEditor, language, onChange, onFocus, value)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Json.Encode as E


codeEditor : List (Html.Attribute msg) -> Html msg
codeEditor attributes =
    Html.node "code-editor" attributes []


value : String -> Html.Attribute msg
value =
    Html.Attributes.property "editorValue" << E.string


language : String -> Html.Attribute msg
language =
    Html.Attributes.property "editorLanguage" << E.string


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.on "editorChanged" <|
        D.map toMsg <|
            D.at [ "target", "editorValue" ] <|
                D.string


onFocus : msg -> Html.Attribute msg
onFocus toMsg =
    Html.Events.on "focus" <|
        D.succeed toMsg


chooseLanguage : String -> String
chooseLanguage contentType =
    case contentType of
        "application/xml" ->
            "xml"

        "text/css" ->
            "css"

        "text/html" ->
            "html"

        "text/html+jinja2" ->
            "jinja2"

        _ ->
            ""
