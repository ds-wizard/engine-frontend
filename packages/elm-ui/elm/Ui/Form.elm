module Ui.Form exposing
    ( Words
    , actions
    , collapsible
    , errorSummary
    , errorSummaryWithWords
    , footer
    , nested
    , readOnly
    , section
    , submitShortcut
    , unsavedBar
    )

{-| Form-level furniture.

`errorSummary` exists because Save is never disabled on validity. A disabled
Save tells the user nothing about what is wrong and leaves a keyboard user on a
dead control; a summary names the problems and can be focused.

-}

import Browser.Events
import Html exposing (Html, details, div, fieldset, li, span, summary, text, ul)
import Html.Attributes exposing (attribute, class, disabled, hidden, id, tabindex)
import Json.Decode as Decode
import Ui.Button as Button exposing (Button)
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Text as Text


{-| The id the summary takes, so a submit handler can move focus to it.
-}
summaryId : String
summaryId =
    "ui-form-errors"


{-| Cmd+Enter on a Mac, Ctrl+Enter elsewhere, submits the form.

A subscription rather than a handler on the fields, because the shortcut should
work from anywhere in the form — including a textarea, where a plain Enter is a
newline and there is otherwise no way to submit from the keyboard at all.

Which modifier is right depends on the platform rather than the browser, and
nothing in the DOM reports the platform reliably, so the caller passes it in.
`metaKey` is Cmd on macOS and the Windows key elsewhere, which is why the two
cannot simply both be accepted.

-}
submitShortcut : { isMac : Bool, onSubmit : msg } -> Sub msg
submitShortcut config =
    Browser.Events.onKeyDown
        (Decode.map3
            (\key meta ctrl ->
                ( config.onSubmit
                , key
                    == "Enter"
                    && (if config.isMac then
                            meta

                        else
                            ctrl
                       )
                )
            )
            (Decode.field "key" Decode.string)
            (Decode.field "metaKey" Decode.bool)
            (Decode.field "ctrlKey" Decode.bool)
            |> Decode.andThen
                (\( msg, matched ) ->
                    if matched then
                        Decode.succeed msg

                    else
                        Decode.fail "not the submit shortcut"
                )
        )


actions : List (Button msg) -> Html msg
actions buttons =
    div [ class "ui-form-actions" ] (List.map Button.toHtml buttons)


{-| The end of a form that creates something: a line under the fields, the way
back at the start and the submit at the end.

The line says the form is over and what follows decides it, so the two buttons
are not read as one more field. Cancel comes first in the document as well as on
screen, and the submit last — as a dialog's footer and the unsaved bar have it:
the eye ends the row on the primary action, and so does the keyboard.

-}
footer : { cancel : Button msg, submit : Button msg } -> Html msg
footer config =
    div [ class "ui-form-footer" ]
        [ Button.toHtml config.cancel
        , Button.toHtml config.submit
        ]


{-| The fields a switch reveals, set under it — two-factor authentication's code
length and expiration under the switch that turns it on.

Indented, with a line down the start edge, so they read as belonging to the
switch above rather than as the next setting in the list, and so the next
setting after them reads as separate again. Put it straight after the switch,
and only while the switch is on.

-}
nested : List (Html msg) -> Html msg
nested =
    div [ class "ui-form-nested" ]


{-| A form shown to somebody who may not change it: every control in it is
disabled, in one place, by a `fieldset` with nothing drawn around it.

Say beside it why it cannot be changed. A disabled control gives no reason, and
is skipped by the keyboard.

-}
readOnly : List (Html msg) -> Html msg
readOnly =
    fieldset [ class "ui-form-readonly", disabled True ]


{-| Settings that belong together, under a name — a page's support contact, its
internal login — as a `fieldset` named by a `legend`, drawn as a box with the
name as its heading and the fields one under another.

The one way a form groups its fields, the same as a role's permission groups,
so a settings page never mixes a card with a title and a fieldset with a legend
for the same job. A group of choices inside a section is not framed again. `id`
is the fieldset's and has to be unique on the page.

-}
section : { id : String, legend : String, content : List (Html msg) } -> Html msg
section config =
    Field.group { id = config.id, legend = config.legend, fields = config.content }
        |> Field.withGroupStacked
        |> Field.groupToHtml


{-| Settings most people never need — an OpenID provider's advanced
configuration — folded away under a label, and unfolded by choosing it.

A native `details` and `summary`: a button that says whether it is expanded,
opened with the keyboard as well as the pointer, and no state for the page to
keep. The view never sets `open`, so a re-render does not fold a section
somebody has opened. It starts folded.

Nothing in it should be required, or able to fail validation, while it is
folded: an error inside it would be out of sight. The contents are set under the
label as `nested` sets them under a switch.

-}
collapsible : { label : String, content : List (Html msg) } -> Html msg
collapsible config =
    details [ class "ui-form-collapsible" ]
        [ summary [ class "ui-form-collapsible__summary" ]
            [ span [ class "ui-form-collapsible__chevron", attribute "aria-hidden" "true" ]
                [ Icon.toHtml (Icon.solid "fa-chevron-right") ]
            , text config.label
            ]
        , nested config.content
        ]


{-| The bar that appears once an existing thing has been changed.

Editing something that already exists has no natural moment to press Save, so
the form says so itself and offers the action where the eye already is. It is
hidden until there is something to save, which is what makes it read as a
consequence of typing rather than as furniture. Hidden rather than absent: it
slides in when it appears and fades out when there is nothing left to save,
instead of vanishing in a frame after a quick save.

Render it into a page template's footer slot. That slot sits below the scrolling
region rather than over it, so the bar is at the bottom of the screen by
construction: nothing is positioned, nothing has to reserve room for it, and
nothing has to be told how wide the sidebar is — which the existing
implementation does with a `calc` repeated three times, once more for the
collapsed sidebar.

It announces itself politely, because a bar appearing at the bottom of a long
form is easy to miss and invisible to anyone not looking at it. Being in the page
all along, it is a live region a screen reader already knows about when it
appears, which is when one is announced reliably.

The actions are rendered in the order given, which is also the tab order. Put
the primary action last — Discard, then Save — as a dialog's footer does: the
eye ends a row on its most important control, and so does the keyboard.

-}
unsavedBar :
    { visible : Bool
    , message : String
    , actions : List (Button msg)
    }
    -> Html msg
unsavedBar config =
    div
        [ class "ui-form-unsaved"
        , attribute "role" "status"
        , hidden (not config.visible)
        ]
        [ span [ class "ui-form-unsaved__message" ] [ text config.message ]
        , div [ class "ui-form-unsaved__actions" ] (List.map Button.toHtml config.actions)
        ]


errorSummary : List String -> Html msg
errorSummary =
    errorSummaryWithWords { errorSummaryTitle = "This form could not be saved" }


{-| What the form furniture says of its own accord: the error summary's title.
`errorSummary` says it in English.
-}
type alias Words =
    { errorSummaryTitle : String
    }


{-| `errorSummary`, with the words given.
-}
errorSummaryWithWords : Words -> List String -> Html msg
errorSummaryWithWords words errors =
    if List.isEmpty errors then
        text ""

    else
        div
            [ class "ui-form-errors"
            , id summaryId
            , tabindex -1
            , attribute "role" "alert"
            ]
            [ Text.subtitle words.errorSummaryTitle
            , ul [ class "ui-form-errors__list" ]
                (List.map (\message -> li [] [ text message ]) errors)
            ]
