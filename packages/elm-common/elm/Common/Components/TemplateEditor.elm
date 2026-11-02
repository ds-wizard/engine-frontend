module Common.Components.TemplateEditor exposing (reference, view)

{-| A Jinja template over an item, as a form field: a switch over it between the
template and what it renders to, and the item's properties under it, each of
which writes its reference where the caret was left.

It is a field like any other — a label, the hint and the warning under it — in
a frame that holds the two bars (`Ui.Field.withFrame`). What the preview shows
is the caller's to fetch; so is remembering where the caret was (`cursor`,
`onCursor`) and putting it back after an insertion (`onInsert` wraps the edit,
and the caller focuses `fieldId` with the caret at `caret` afterwards).

-}

import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Ui.Button as Button
import Ui.Field as Field
import Ui.ModeSwitch as ModeSwitch


view :
    Gettext.Locale
    ->
        { id : String
        , label : String
        , value : String
        , onInput : String -> msg
        , hint : Maybe (List (Html msg))
        , warning : Maybe String
        , preview : Maybe (Html msg)
        , onTemplate : msg
        , onPreview : msg
        , properties : List String
        , cursor : Maybe ( Int, Int )
        , onCursor : ( Int, Int ) -> msg
        , onInsert : { fieldId : String, caret : Int } -> msg -> msg
        }
    -> Html msg
view locale config =
    let
        previewing =
            Maybe.isJust config.preview

        -- Where the insertion goes, and where the caret should be left: right
        -- after it, in the box, so the next thing typed follows it.
        insert property =
            let
                ref =
                    reference "item" property

                at =
                    case config.cursor of
                        Just ( start, _ ) ->
                            start

                        Nothing ->
                            String.length config.value
            in
            config.onInsert
                { fieldId = config.id, caret = at + String.length ref }
                (config.onInput (String.left at config.value ++ ref ++ String.dropLeft at config.value))

        switch =
            ModeSwitch.toHtml
                { label = config.label
                , options =
                    [ ModeSwitch.option
                        { label = gettext "Template" locale
                        , chosen = not previewing
                        , onChoose = config.onTemplate
                        }
                        |> ModeSwitch.withTestId (config.id ++ "_mode_template")
                    , ModeSwitch.option
                        { label = gettext "Preview" locale
                        , chosen = previewing
                        , onChoose = config.onPreview
                        }
                        |> ModeSwitch.withTestId (config.id ++ "_mode_preview")
                    ]
                }

        properties =
            if previewing then
                []

            else
                List.map (\property -> Button.toHtml (Button.small (Button.ghost property (insert property)))) config.properties

        withHint field =
            case config.hint of
                Just hint ->
                    Field.withHintHtml hint field

                Nothing ->
                    field
    in
    Field.textarea
        { id = config.id
        , label = config.label
        , value = config.value
        , onInput = config.onInput
        }
        |> Field.withRows (max 4 (List.length (String.lines config.value)))
        |> Field.withSelectionOnBlur config.onCursor
        |> Field.withWarning config.warning
        |> withHint
        |> Field.withFrame { header = [ switch ], footer = properties, instead = config.preview }
        |> Field.toHtml


{-| How a template refers to a property of `domain`: `{{ item.name }}`, or
`{{ item["a name"] }}` for a name that is not a plain identifier.
-}
reference : String -> String -> String
reference domain name =
    if String.all (\char -> Char.isAlphaNum char || char == '_') name && not (String.isEmpty name) then
        "{{ " ++ domain ++ "." ++ name ++ " }}"

    else
        "{{ " ++ domain ++ "[\"" ++ name ++ "\"] }}"
