module Ui.TagInput exposing
    ( TagInput
    , focusInput
    , tagInput
    , toHtml
    , withDisabled
    , withError
    , withHint
    , withSuggestions
    )

{-| A field whose value is a set of free-text tags: a project's tags.

It is `Ui.Field` in shape — label, control, hint, error, and the
`aria-describedby` between them built here rather than by the caller — with the
chosen tags under the control as `Ui.Tag`s, each with its own remove button.

Three decisions worth knowing:

  - **Enter adds, and so does a button.** Enter is what everyone tries first;
    the button is what makes adding discoverable, and reachable by a pointer
    without learning a key.
  - **Enter does nothing while a word is being composed.** Typing Japanese or
    Chinese, Enter is how a candidate is accepted inside the input method, and
    a field that treats that press as "add" adds half a word. The browser says
    so with `isComposing`, and Safari — which reports the committing press
    after composition has ended — with key code 229.
  - **The field does not deduplicate, trim beyond whitespace, or validate.**
    What counts as the same tag is the caller's rule, and the caller owns the
    list.

-}

import Browser.Dom as Dom
import Html exposing (Html, div, label, p, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList, for, id, type_, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Task
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Tag as Tag


type TagInput msg
    = TagInput (Config msg)


type alias Config msg =
    { id : String
    , label : String
    , values : List String
    , input : String
    , onInput : String -> msg
    , onAdd : String -> msg
    , onRemove : String -> msg
    , addLabel : String
    , removeLabel : String -> String
    , hint : Maybe String
    , error : Maybe String
    , suggestions : List String
    , disabled : Bool
    }


{-| `input` is what is typed and not yet added; `values` are the tags.

`onAdd` is given the typed text with the surrounding whitespace removed, and is
never given an empty string. `removeLabel` names each remove button after its
tag — `\tag -> "Remove tag " ++ tag`.

-}
tagInput :
    { id : String
    , label : String
    , values : List String
    , input : String
    , onInput : String -> msg
    , onAdd : String -> msg
    , onRemove : String -> msg
    , addLabel : String
    , removeLabel : String -> String
    }
    -> TagInput msg
tagInput config =
    TagInput
        { id = config.id
        , label = config.label
        , values = config.values
        , input = config.input
        , onInput = config.onInput
        , onAdd = config.onAdd
        , onRemove = config.onRemove
        , addLabel = config.addLabel
        , removeLabel = config.removeLabel
        , hint = Nothing
        , error = Nothing
        , suggestions = []
        , disabled = False
        }


withHint : String -> TagInput msg -> TagInput msg
withHint hint (TagInput config) =
    TagInput { config | hint = Just hint }


withError : Maybe String -> TagInput msg -> TagInput msg
withError error (TagInput config) =
    TagInput { config | error = error }


{-| Tags already in use elsewhere, offered as the user types.

The same list `Ui.Field.withSuggestions` draws under a text field — `ui-suggest`
(`packages/elm-ui/js/suggest-input.js`), so a project's tags are offered the way an
affiliation is: it filters as the text is typed, the arrow keys move through it,
Enter takes what they are on and Escape closes it.

Choosing one fills the input rather than adding the tag, as it does in a text
field, so a suggestion is confirmed the same way as anything typed: Enter again,
or the button. The Enter that chooses is the list's alone — it does not also add
what was typed so far.

The list may change while the field is in use — a caller fetching tags for what
is being typed — and it opens when the answer arrives, unless it was dismissed.

-}
withSuggestions : List String -> TagInput msg -> TagInput msg
withSuggestions suggestions (TagInput config) =
    TagInput { config | suggestions = suggestions }


{-| Disabled, the tags are still shown but lose their remove buttons: a button
that cannot be pressed in a place nothing can be changed is only noise between
the words.
-}
withDisabled : Bool -> TagInput msg -> TagInput msg
withDisabled disabled (TagInput config) =
    TagInput { config | disabled = disabled }


{-| Put focus back in the field's text input.

Call it when handling `onRemove`. The remove button that had focus goes with its
tag, and focus on an element that no longer exists lands on the document — a
keyboard user is sent back to the top of the page for removing a word.

-}
focusInput : msg -> String -> Cmd msg
focusInput noop fieldId =
    Task.attempt (always noop) (Dom.focus fieldId)


toHtml : TagInput msg -> Html msg
toHtml (TagInput config) =
    let
        typed =
            String.trim config.input
    in
    div [ class "ui-field" ]
        [ label [ class "ui-field__label", id (labelId config), for config.id ] [ text config.label ]
        , div [ class "ui-tag-input__entry" ]
            [ -- Always wrapped, with or without suggestions: they usually
              -- arrive while the field is being typed into, and a wrapper
              -- that appeared with them would rebuild the input under the
              -- caret and drop focus mid-word.
              Html.node "ui-suggest"
                [ class "ui-suggest ui-tag-input__text"
                , attribute "suggestions" (Encode.encode 0 (Encode.list Encode.string config.suggestions))
                , attribute "label" config.label
                ]
                [ Html.input
                    ([ type_ "text"
                     , id config.id
                     , classList
                        [ ( "ui-input", True )
                        , ( "is-invalid", Maybe.isJust config.error )
                        ]
                     , value config.input
                     , Attr.disabled config.disabled

                     -- The platform's autocomplete would offer whatever was
                     -- typed into any field of this name before, on top of the
                     -- suggestions.
                     , Attr.autocomplete False
                     , onInput config.onInput
                     , onEnter typed (config.onAdd typed)
                     ]
                        ++ presentIf (not (String.isEmpty (describedBy config))) (attribute "aria-describedby" (describedBy config))
                        ++ presentIf (Maybe.isJust config.error) (attribute "aria-invalid" "true")
                    )
                    []
                ]
            , Button.toHtml
                (Button.secondary config.addLabel (config.onAdd typed)
                    |> Button.withIcon (Icon.solid "fa-plus")
                    -- Nothing to add is a state the button can show; a press
                    -- that does nothing is not.
                    |> Button.withDisabled (config.disabled || String.isEmpty typed)
                    |> Button.withTestId (config.id ++ "_add")
                )
            ]
        , case config.hint of
            Just hint ->
                p [ class "ui-field__hint", id (hintId config) ] [ text hint ]

            Nothing ->
                text ""
        , case config.error of
            Just error ->
                p [ class "ui-field__error", id (errorId config) ]
                    [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                    , text error
                    ]

            Nothing ->
                text ""
        , if List.isEmpty config.values then
            text ""

          else
            -- Named by the field's label, so arriving at the list says whose
            -- tags these are rather than only how many.
            div [ class "ui-tag-input__tags", attribute "role" "group", attribute "aria-labelledby" (labelId config) ]
                [ Tag.list (List.map (viewTag config) config.values) ]
        ]


viewTag : Config msg -> String -> Tag.Tag msg
viewTag config value =
    let
        tag =
            Tag.tag value
                |> Tag.withTestId (config.id ++ "_tag_" ++ value)
    in
    if config.disabled then
        tag

    else
        tag
            |> Tag.withRemove { label = config.removeLabel value, onRemove = config.onRemove value }


{-| Enter adds what is typed, unless an input method is mid-composition or there
is nothing to add.

A keydown, which is where Enter acts on every native control; WCAG 2.5.2 is
about pointer presses and does not reach it. Prevented only when it adds, so an
Enter in an empty field still does what Enter does in a form. An Enter that is
choosing a suggestion never gets here: `ui-suggest` takes it on the way down.

-}
onEnter : String -> msg -> Html.Attribute msg
onEnter typed msg =
    preventDefaultOn "keydown"
        (Decode.map3
            (\key composing keyCode ->
                key == "Enter" && not composing && keyCode /= 229 && not (String.isEmpty typed)
            )
            (Decode.field "key" Decode.string)
            (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
            (Decode.oneOf [ Decode.field "keyCode" Decode.int, Decode.succeed 0 ])
            |> Decode.andThen
                (\adds ->
                    if adds then
                        Decode.succeed ( msg, True )

                    else
                        Decode.fail "not an add"
                )
        )


describedBy : Config msg -> String
describedBy config =
    [ Maybe.map (always (hintId config)) config.hint
    , Maybe.map (always (errorId config)) config.error
    ]
        |> Maybe.values
        |> String.join " "


presentIf : Bool -> Html.Attribute msg -> List (Html.Attribute msg)
presentIf condition attr =
    if condition then
        [ attr ]

    else
        []


labelId : Config msg -> String
labelId config =
    config.id ++ "-label"


hintId : Config msg -> String
hintId config =
    config.id ++ "-hint"


errorId : Config msg -> String
errorId config =
    config.id ++ "-error"
