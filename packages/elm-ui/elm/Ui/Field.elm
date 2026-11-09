module Ui.Field exposing
    ( Choice
    , ChoiceTone(..)
    , Field
    , FieldGroup
    , MarkdownWords
    , Strength
    , StrengthTone(..)
    , Words
    , checkbox
    , checkboxGroup
    , choice
    , choiceValue
    , code
    , color
    , date
    , dateTime
    , group
    , groupToHtml
    , input
    , markdown
    , password
    , radioGroup
    , readOnly
    , readOnlyHtml
    , secret
    , select
    , selectWithGroups
    , strengthChecking
    , strengthLabel
    , strengthLabelWithWords
    , strengthScored
    , strengthScoredAs
    , strengthUnavailable
    , textarea
    , time
    , toHtml
    , toggle
    , withAutocomplete
    , withChoiceBadge
    , withChoiceColor
    , withChoiceDescription
    , withChoiceIcon
    , withChoiceTone
    , withChoicesInline
    , withClear
    , withCodeWrap
    , withDisabled
    , withDisabledReason
    , withError
    , withFrame
    , withGroupActions
    , withGroupError
    , withGroupHeaderAction
    , withGroupHint
    , withGroupHintHtml
    , withGroupStacked
    , withHiddenLabel
    , withHint
    , withHintHtml
    , withIdPrefix
    , withInputMode
    , withLabelAction
    , withLabelBadge
    , withLabelHtml
    , withOnBlur
    , withOnFocus
    , withPlaceholder
    , withPrefix
    , withRequired
    , withRows
    , withSelectionOnBlur
    , withStrength
    , withStrengthOf
    , withSubmitOnEnter
    , withSubmitShortcut
    , withSuffix
    , withSuffixIcon
    , withSuggestions
    , withSwatches
    , withWarning
    , withWords
    )

{-| A form field builds its own label, control, hint and error association.

The caller never assembles the parts, so `aria-describedby`, `aria-invalid`, the
generated id and the label's `for` cannot be forgotten — they are not the
caller's to forget.

The field is library-agnostic: it takes an error as a value, so it works with
`enbala/elm-form`, with a hand-rolled model, or with anything else. A thin
adapter bridges whichever is in use.

-}

import Color exposing (Color)
import Html exposing (Html, div, fieldset, label, legend, option, p, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList, for, id, selected, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onFocus, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Rumkin
import Ui.Badge as Badge exposing (Badge)
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId
import Ui.Theme.Oklch as Oklch
import Ui.Tooltip as Tooltip


type Field msg
    = Field (Config msg)


type alias Config msg =
    { id : String
    , label : String
    , control : Control msg
    , hint : Maybe (List (Html msg))
    , labelHtml : Maybe (List (Html msg))
    , error : Maybe String
    , warning : Maybe String
    , required : Bool
    , disabled : Bool
    , placeholder : Maybe String
    , hiddenLabel : Bool
    , labelAction : Maybe (Html msg)
    , prefix : Maybe String
    , suffix : Maybe String
    , autocomplete : Maybe String
    , onSubmitShortcut : Maybe msg
    , submitOnEnter : Bool
    , rows : Maybe Int
    , strength : StrengthSource
    , swatches : List String
    , clearable : Bool
    , suggestions : List String
    , inputMode : Maybe String
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    , onSelectionBlur : Maybe (( Int, Int ) -> msg)
    , words : Words
    , labelBadge : Maybe (Badge msg)
    , suffixIcon : Maybe Icon
    , disabledReason : Maybe { id : String, reason : String }
    , choicesInline : Bool
    , codeWrap : Bool
    , frame : Maybe (Frame msg)
    }


{-| A box round the control with a bar above and below it: see `withFrame`.
-}
type alias Frame msg =
    { header : List (Html msg)
    , footer : List (Html msg)
    , instead : Maybe (Html msg)
    }


{-| What a field says of its own accord, as opposed to what it is told to say.

  - `show` and `pick` name the secret field's reveal button and the color
    field's picker; `clear` names the date field's clear button, and the color
    field's when it has one. Each is given
    the field's label.
  - `invalidDate` is the message for a stored date the picker cannot read, given
    the value.
  - `checking` is what a password meter says while a supplied strength is being
    worked out, and `strength` the five verdicts of the local one.
  - `markdown` is the markdown editor's toolbar.

Every field says these in English until `withWords` gives it the page's
language.

-}
type alias Words =
    { show : String -> String
    , pick : String -> String
    , clear : String -> String
    , invalidDate : String -> String
    , checking : String
    , strength :
        { veryWeak : String
        , weak : String
        , reasonable : String
        , strong : String
        , veryStrong : String
        }
    , markdown : MarkdownWords
    }


{-| The markdown editor's toolbar: a name for every button, the two prompts,
their buttons, the mode switch and its two modes. `toolbar` is the toolbar's
whole name, given the field's label — "Description formatting".
-}
type alias MarkdownWords =
    { heading2 : String
    , heading3 : String
    , bold : String
    , italic : String
    , strikethrough : String
    , link : String
    , image : String
    , bulletList : String
    , numberedList : String
    , inlineCode : String
    , codeBlock : String
    , quote : String
    , linkAddress : String
    , imageAddress : String
    , apply : String
    , cancel : String
    , editingMode : String
    , formatted : String
    , markdown : String
    , toolbar : String -> String
    }


english : Words
english =
    { show = \label -> "Show " ++ String.toLower label
    , pick = \label -> "Pick " ++ String.toLower label
    , clear = \label -> "Clear " ++ String.toLower label
    , invalidDate = \value -> "The stored value \"" ++ value ++ "\" is not a valid date."
    , checking = "Checking…"
    , strength =
        { veryWeak = "Very weak"
        , weak = "Weak"
        , reasonable = "Reasonable"
        , strong = "Strong"
        , veryStrong = "Very strong"
        }
    , markdown =
        { heading2 = "Heading 2"
        , heading3 = "Heading 3"
        , bold = "Bold"
        , italic = "Italic"
        , strikethrough = "Strikethrough"
        , link = "Link"
        , image = "Image"
        , bulletList = "Bullet list"
        , numberedList = "Numbered list"
        , inlineCode = "Inline code"
        , codeBlock = "Code block"
        , quote = "Quote"
        , linkAddress = "Link address"
        , imageAddress = "Image address"
        , apply = "Apply"
        , cancel = "Cancel"
        , editingMode = "Editing mode"
        , formatted = "Formatted"
        , markdown = "Markdown"
        , toolbar = \label -> label ++ " formatting"
        }
    }


{-| Where a password field's strength comes from.

`Local` is Rumkin, in the browser, which is what the existing sign-up forms use.
`Supplied` is for the ones that ask the server — checked against
an API, debounced, and the answer arrives later or not at all. A field that can
only compute its own strength cannot show either of those.

-}
type StrengthSource
    = NoStrength
    | Local
    | Supplied Strength


{-| A strength somebody else worked out.

`level` out of `max`, so a score of 4 out of 4 and one of 5 out of 5 both fill
the bar, and the words are the caller's: what counts as strong is a policy, not
a component's opinion.

-}
type Strength
    = Checking { max : Int }
    | Scored { level : Int, max : Int, label : String, tone : Maybe StrengthTone }
    | Unavailable { max : Int, label : String }


{-| The color of a verdict, where the caller's policy decides it rather than
the fraction of the bar (`strengthScoredAs`).
-}
type StrengthTone
    = WeakTone
    | FairTone
    | StrongTone


type Control msg
    = Text String (String -> msg)
    | Multiline String (String -> msg)
    | Options (List ( String, String )) String (String -> msg)
      -- The same, with the options in named groups: a list long enough that
      -- finding one means knowing which part of it to look in.
    | GroupedOptions (List ( String, String )) (List ( String, List ( String, String ) )) String (String -> msg)
    | Check Bool (Bool -> msg)
    | Moment Kind String (String -> msg)
    | Markup String (String -> msg)
    | Code String String (String -> msg)
    | Switch Bool (Bool -> msg)
    | Password String (String -> msg)
    | Secret { value : String, onInput : String -> msg, revealed : Bool, onReveal : Bool -> msg }
    | Swatch String (String -> msg)
    | Static (List (Html msg))
    | Radios (List (Choice msg)) String (String -> msg)
    | Checks (List (Choice msg)) (List String) (String -> msg)


{-| One option in a radio or checkbox group.

A description is a sentence about the option, not a second label: it is inside
the option's own `label`, so it is read as part of choosing rather than as
something nearby.

-}
type Choice msg
    = Choice
        { value : String
        , label : String
        , description : Maybe String
        , icon : Maybe Icon
        , color : Maybe String
        , badge : Maybe String
        , tone : Maybe ChoiceTone
        }


{-| The status an option stands for, drawn in that status's colors — an
announcement's level as the alert it will show.
-}
type ChoiceTone
    = InfoTone
    | WarningTone
    | DangerTone


{-| What a date field is for, which decides both what the picker offers and what
the value means.

The value is always the machine-readable one and never the reader's rendering of
it: a date is a calendar date (`2026-09-11`), a time is a wall-clock time
(`14:30`), and a date _and_ time is an instant, which is only unambiguous with a
zone — so it is ISO-8601 with one, and the field shows it in the browser's.

-}
type Kind
    = Day
    | DayTime
    | Clock


base : String -> String -> Control msg -> Field msg
base fieldId labelText control =
    Field
        { id = fieldId
        , label = labelText
        , control = control
        , hint = Nothing
        , labelHtml = Nothing
        , error = Nothing
        , warning = Nothing
        , required = False
        , disabled = False
        , placeholder = Nothing
        , hiddenLabel = False
        , labelAction = Nothing
        , prefix = Nothing
        , suffix = Nothing
        , autocomplete = Nothing
        , onSubmitShortcut = Nothing
        , submitOnEnter = False
        , rows = Nothing
        , strength = NoStrength
        , swatches = []
        , clearable = False
        , suggestions = []
        , inputMode = Nothing
        , onFocus = Nothing
        , onBlur = Nothing
        , onSelectionBlur = Nothing
        , words = english
        , labelBadge = Nothing
        , suffixIcon = Nothing
        , disabledReason = Nothing
        , choicesInline = False
        , codeWrap = True
        , frame = Nothing
        }


input : { id : String, label : String, value : String, onInput : String -> msg } -> Field msg
input config =
    base config.id config.label (Text config.value config.onInput)


textarea : { id : String, label : String, value : String, onInput : String -> msg } -> Field msg
textarea config =
    base config.id config.label (Multiline config.value config.onInput)


{-| A select whose options are in named groups — a question picked out of a
knowledge model, under the chapter it belongs to.

`loose` are the options that belong to no group and come first: the "choose
one" entry a select opens on. Groups render as `optgroup`, which every browser
and every screen reader already understands as "these belong together", and
which nothing else in a select can say.

A group with nothing in it is the caller's to leave out.

-}
selectWithGroups :
    { id : String
    , label : String
    , value : String
    , loose : List ( String, String )
    , groups : List ( String, List ( String, String ) )
    , onInput : String -> msg
    }
    -> Field msg
selectWithGroups config =
    base config.id config.label (GroupedOptions config.loose config.groups config.value config.onInput)


select :
    { id : String
    , label : String
    , value : String
    , options : List ( String, String )
    , onInput : String -> msg
    }
    -> Field msg
select config =
    base config.id config.label (Options config.options config.value config.onInput)


{-| A date.

The picker is flatpickr, wrapped in a custom element — but the field it produces
is typed into like any other, which is the part that matters: the calendar is a
shortcut, not the only way in. See `packages/elm-ui/js/date-picker.js`.

-}
date : { id : String, label : String, value : String, onChange : String -> msg } -> Field msg
date config =
    base config.id config.label (Moment Day config.value config.onChange)


dateTime : { id : String, label : String, value : String, onChange : String -> msg } -> Field msg
dateTime config =
    base config.id config.label (Moment DayTime config.value config.onChange)


time : { id : String, label : String, value : String, onChange : String -> msg } -> Field msg
time config =
    base config.id config.label (Moment Clock config.value config.onChange)


{-| Code, with highlighting and a gutter.

`language` is one of the names `Ui.CodeBlock` lists; anything else is plain
text, which is what an unknown language should look like rather than an error.

-}
code :
    { id : String, label : String, language : String, value : String, onChange : String -> msg }
    -> Field msg
code config =
    base config.id config.label (Code config.language config.value config.onChange)


{-| Markdown, written either as formatted text or as the source.

The editor is tiptap behind a custom element, and the same rule applies as to
the date field: the toolbar is one tab stop with the arrow keys along it, so a
dozen formatting buttons do not stand between the label and the text. See
`packages/elm-ui/js/markdown-editor.js`.

-}
markdown : { id : String, label : String, value : String, onChange : String -> msg } -> Field msg
markdown config =
    base config.id config.label (Markup config.value config.onChange)


{-| An on/off setting that takes effect as it is switched, rather than a
checkbox that takes effect when the form is saved.

A checkbox with `role="switch"`, so it is announced as "on" or "off" rather than
"checked", and so it stays a real checkbox to everything that does not know the
role. The state shows as a position as well as a color (1.4.1).

-}
toggle : { id : String, label : String, checked : Bool, onCheck : Bool -> msg } -> Field msg
toggle config =
    base config.id config.label (Switch config.checked config.onCheck)


{-| A password.

`withStrength` adds a meter. The meter is announced: the existing one is five
colored bars and nothing else, which tells a screen reader nothing and tells
anyone who cannot separate red from green nothing either (1.4.1).

Pair it with `withAutocomplete "new-password"` or `"current-password"` — a
password field a manager cannot recognize is one people work around (1.3.5).

-}
password : { id : String, label : String, value : String, onInput : String -> msg } -> Field msg
password config =
    base config.id config.label (Password config.value config.onInput)


{-| A value that is hidden but can be looked at: an API key, a client secret.

The reveal state is the caller's, because it belongs to the page rather than to
the field — a form that reveals one secret should not reveal the rest, and
whether it survives a re-render is a decision only the page can make.

-}
secret :
    { id : String
    , label : String
    , value : String
    , onInput : String -> msg
    , revealed : Bool
    , onReveal : Bool -> msg
    }
    -> Field msg
secret config =
    base config.id
        config.label
        (Secret
            { value = config.value
            , onInput = config.onInput
            , revealed = config.revealed
            , onReveal = config.onReveal
            }
        )


{-| A color, chosen from a set or typed as a hex.

The swatches are a radio group, and each one is named by its value: a color is
not an accessible name, so a row of colored squares with nothing else is a row
of unlabeled buttons. The text input beside them is what makes any other color
reachable, and what makes the current one readable.

-}
color : { id : String, label : String, value : String, onInput : String -> msg } -> Field msg
color config =
    base config.id config.label (Swatch config.value config.onInput)


{-| Colors worth one press, beside the field, or under it where the row is too
narrow for both.

Optional, and after rather than before: most color fields have no palette, and
where there is one it is a shortcut to something the picker and the hex can both
reach anyway — so it comes after the control rather than in front of it. The
swatches are round, so they are not taken for the picker, which is square.

-}
withSwatches : List String -> Field msg -> Field msg
withSwatches swatches (Field config) =
    Field { config | swatches = swatches }


{-| A button that empties a color field, for a color that may be left empty —
one with a default, which the placeholder then shows again.

The date field's button: the same glyph at the end of the input, the same name,
and only while there is something to clear. Pressed, it gives focus back to the
input (`js/color-field.js`), since the button itself is gone.

-}
withClear : Field msg -> Field msg
withClear (Field config) =
    Field { config | clearable = True }


{-| A value shown in the shape of a field, but not editable.

Not a disabled input: a disabled input says "you could change this, but not
now", and this says "this is what it is". It is text, so it is not a tab stop
and cannot be mistaken for something to fill in.

-}
readOnly : { id : String, label : String, value : String } -> Field msg
readOnly config =
    base config.id config.label (Static [ text config.value ])


{-| The same, for a value that is a link, a badge, or anything else that is not
a string.
-}
readOnlyHtml : { id : String, label : String, value : List (Html msg) } -> Field msg
readOnlyHtml config =
    base config.id config.label (Static config.value)


{-| One of a set. The options are native radios, so the arrow keys move between
them and the group is one tab stop without anything being written to make it so.
-}
radioGroup :
    { id : String, label : String, value : String, options : List (Choice msg), onChange : String -> msg }
    -> Field msg
radioGroup config =
    base config.id config.label (Radios config.options config.value config.onChange)


{-| Any number of a set. Each box is its own tab stop, which is the difference
between a checkbox group and a radio group and is also the native behavior —
several independent yes/no answers, not one answer with several shapes.

`onToggle` is given the value that was pressed; the caller owns the set.

-}
checkboxGroup :
    { id : String, label : String, values : List String, options : List (Choice msg), onToggle : String -> msg }
    -> Field msg
checkboxGroup config =
    base config.id config.label (Checks config.options config.values config.onToggle)


choice : { value : String, label : String } -> Choice msg
choice config =
    Choice
        { value = config.value
        , label = config.label
        , description = Nothing
        , icon = Nothing
        , color = Nothing
        , badge = Nothing
        , tone = Nothing
        }


{-| The value an option stands for.

For an adapter that keeps a checkbox group's answers somewhere other than one
list — `enbala/elm-form` keeps one boolean per option — and so has to ask each
option which one it is.

-}
choiceValue : Choice msg -> String
choiceValue (Choice config) =
    config.value


withChoiceDescription : String -> Choice msg -> Choice msg
withChoiceDescription description (Choice config) =
    Choice { config | description = Just description }


withChoiceIcon : Icon -> Choice msg -> Choice msg
withChoiceIcon icon (Choice config) =
    Choice { config | icon = Just icon }


{-| The option drawn in a status's colors — the tint, edge and text of the
alert it stands for — so "Warning" looks like the warning it will produce. The
label still says which it is, and the selected option is marked by its input and
a stronger edge, never by the color alone (1.4.1).
-}
withChoiceTone : ChoiceTone -> Choice msg -> Choice msg
withChoiceTone tone (Choice config) =
    Choice { config | tone = Just tone }


{-| A radio or checkbox group's options side by side, sharing the row, and
wrapping only when there is not room for them — for a few short options such as
an announcement's level.
-}
withChoicesInline : Field msg -> Field msg
withChoicesInline (Field config) =
    Field { config | choicesInline = True }


{-| A dot of the option's color before its label — a project tag, a phase.

Decorative, and hidden from assistive technology: the color is never what tells
options apart, the label is (1.4.1). The value is a CSS color, used as given.

-}
withChoiceColor : String -> Choice msg -> Choice msg
withChoiceColor colorValue (Choice config) =
    Choice { config | color = Just colorValue }


{-| A small neutral badge after the option's label — "default", "recommended".

Inside the option's `label`, like the description, so it is read as part of the
option's name ("English, default") rather than as text somewhere near it.

-}
withChoiceBadge : String -> Choice msg -> Choice msg
withChoiceBadge badgeLabel (Choice config) =
    Choice { config | badge = Just badgeLabel }


checkbox : { id : String, label : String, checked : Bool, onCheck : Bool -> msg } -> Field msg
checkbox config =
    base config.id config.label (Check config.checked config.onCheck)


withHint : String -> Field msg -> Field msg
withHint hint (Field config) =
    Field { config | hint = Just [ text hint ] }


{-| A hint with something in it that plain text cannot say — a name in code, an
emphasis, a link. `Common.Components.Form.Field.withHintMarkdown` renders the
markdown the existing settings are written in; a caller with no markdown should
use `withHint`, which parses nothing.
-}
withHintHtml : List (Html msg) -> Field msg -> Field msg
withHintHtml hint (Field config) =
    Field { config | hint = Just hint }


{-| A checkbox's or a switch's label as a sentence with links in it — "I have
read the Privacy Policy and the Terms of Service". The string label is kept for
everything that needs words; the label shown is this. A link inside a label
is followed, not ticked: clicking interactive content in a `label` does not
activate its control. Other kinds of field ignore it.
-}
withLabelHtml : List (Html msg) -> Field msg -> Field msg
withLabelHtml labelHtml (Field config) =
    Field { config | labelHtml = Just labelHtml }


{-| A badge beside the label — a plugin's version after its name. Inside the
label, so it is part of the field's name ("Excel export 1.2.0") and is read with
it, not as something nearby.
-}
withLabelBadge : Badge msg -> Field msg -> Field msg
withLabelBadge badge (Field config) =
    Field { config | labelBadge = Just badge }


{-| Put `prefix` in front of the field's id, and so in front of every id derived
from it — the label's, the hint's, the error's.

For a form that is on the page twice, such as a create and an edit dialog built
from the same fields: without it both carry the same ids, and a label or a
description can point at the other form's control.

-}
withIdPrefix : String -> Field msg -> Field msg
withIdPrefix prefix (Field config) =
    Field { config | id = prefix ++ config.id }


withError : Maybe String -> Field msg -> Field msg
withError error (Field config) =
    Field { config | error = error }


{-| Something the reader should know before changing this field, when the value
it holds is perfectly valid — "Changing the type will remove all the answers".
Not an error: nothing is wrong, the control is not `aria-invalid`, and the form
can be submitted. It reads like one otherwise, with an icon of its own, because
what it is about is the same: a consequence of the value.
-}
withWarning : Maybe String -> Field msg -> Field msg
withWarning warning (Field config) =
    Field { config | warning = warning }


withRequired : Bool -> Field msg -> Field msg
withRequired required (Field config) =
    Field { config | required = required }


withDisabled : Bool -> Field msg -> Field msg
withDisabled disabled (Field config) =
    Field { config | disabled = disabled }


{-| A select that cannot be changed, and says why — the last owner's
permission in the share dialog.

A `disabled` select cannot take focus, so a reason put on it would reach a
pointer and nothing else; an `aria-disabled` one is still a select, and its
value can still be changed from its list. So this draws the select's shape as
a button instead: the value and the chevron, the disabled look, `aria-disabled`,
and nothing on a press. It stays in the tab order, is named by the field's label
and its value together, and the reason is its tooltip on hover and on focus and
its description, as `Ui.Button.withDisabledReason` does for a button. `id` is
the tooltip's and has to be unique on the page.

Only a select takes it; on any other field this does nothing.

-}
withDisabledReason : { id : String, reason : String } -> Field msg -> Field msg
withDisabledReason reason (Field config) =
    Field { config | disabledReason = Just reason }


withPlaceholder : String -> Field msg -> Field msg
withPlaceholder placeholder (Field config) =
    Field { config | placeholder = Just placeholder }


{-| How many lines a box shows, instead of the height it has by default.

In lines rather than in a length, so a reader who sets a larger text size or a
looser line height (WCAG 1.4.12) gets a box that grew with them. A box that
should follow what is typed into it takes the line count of its own value; a
box counts lines, not wrapped rows, which is what the wizard did and is what
makes it stop growing when the typing stops.

-}
withRows : Int -> Field msg -> Field msg
withRows count (Field config) =
    Field { config | rows = Just count }


{-| Whether a code field wraps its long lines, which it does by default.

A field in a form has no room to scroll sideways in, so wrapping is what it
wants. An editor that fills the screen is the other case: code is written in
lines, and a file whose lines are folded back on themselves is a file whose
indentation stops meaning anything — so an editor offers the choice and this
carries it. Turning it off keeps the document, the cursor and the scroll
position; only the wrapping is reconfigured.

-}
withCodeWrap : Bool -> Field msg -> Field msg
withCodeWrap wrap (Field config) =
    Field { config | codeWrap = wrap }


{-| The control in a box with a bar above it and one below — for a field that
comes with its own tools, as a template does with a switch to its preview over
it and the values it can use under it. The box is the frame: the control in it
draws no border of its own, and the focus ring goes round the box.

`instead` is shown in the box in place of the control — the preview — while the
control stays in the document, hidden, so the label still names something and
what was typed is kept. Either bar is left out when it is empty.

-}
withFrame : { header : List (Html msg), footer : List (Html msg), instead : Maybe (Html msg) } -> Field msg -> Field msg
withFrame frame (Field config) =
    Field { config | frame = Just frame }


{-| Cmd/Ctrl+Enter in this box sends it.

For a box that is a form on its own — a comment, a reply — where the page's own
submit shortcut has no way of telling which of several boxes the cursor is in.
On a `textarea` Enter is a new line and has to stay one, so the modifier is what
sends it.

Either modifier is taken, rather than the platform's own: the box cannot see
which platform it is on, and Ctrl+Enter on a Mac is not otherwise spoken for.

-}
withSubmitShortcut : msg -> Field msg -> Field msg
withSubmitShortcut onSubmit (Field config) =
    Field { config | onSubmitShortcut = Just onSubmit, submitOnEnter = False }


{-| Enter in this box sends it, and Shift+Enter makes a new line — the way a
chat box works, for a box where a message is mostly one line and sending is
what it is for.

Cmd/Ctrl+Enter sends as well. Enter that confirms a word an input method is
composing stays the input method's.

-}
withSubmitOnEnter : msg -> Field msg -> Field msg
withSubmitOnEnter onSubmit (Field config) =
    Field { config | onSubmitShortcut = Just onSubmit, submitOnEnter = True }


{-| Enter with a modifier, and nothing else — so the key that makes a new line
still makes one. With `submitOnEnter`, Enter alone too, and Shift+Enter is the
new line.
-}
onSubmitKey : Bool -> msg -> Html.Attribute msg
onSubmitKey submitOnEnter onSubmit =
    let
        matches key meta ctrl shift composing =
            key
                == "Enter"
                && not composing
                && (meta || ctrl || (submitOnEnter && not shift))
    in
    preventDefaultOn "keydown"
        (Decode.map5
            (\key meta ctrl shift composing -> ( onSubmit, matches key meta ctrl shift composing ))
            (Decode.field "key" Decode.string)
            (Decode.field "metaKey" Decode.bool)
            (Decode.field "ctrlKey" Decode.bool)
            (Decode.field "shiftKey" Decode.bool)
            (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
            |> Decode.andThen
                (\( msg, matched ) ->
                    if matched then
                        Decode.succeed ( msg, True )

                    else
                        Decode.fail "not the submit key"
                )
        )


{-| Something in front of the value, or after it: a unit, a currency, a computed
equivalent. It is part of the field's description rather than decoration, so it
is named in `aria-describedby` — "10, megabytes" rather than "10".
-}
withPrefix : String -> Field msg -> Field msg
withPrefix prefix (Field config) =
    Field { config | prefix = Just prefix }


withSuffix : String -> Field msg -> Field msg
withSuffix suffix (Field config) =
    Field { config | suffix = Just suffix }


{-| An icon after the control, in the same addon a suffix sits in — the icon a
field's value names, drawn as it is typed. A preview, not a description: it is
hidden from assistive technology, where the value already says what it is.
-}
withSuffixIcon : Icon -> Field msg -> Field msg
withSuffixIcon icon (Field config) =
    Field { config | suffixIcon = Just icon }


{-| What a password manager should put here — `"new-password"`,
`"current-password"`, `"email"`, `"one-time-code"` (1.3.5).
-}
withAutocomplete : String -> Field msg -> Field msg
withAutocomplete token (Field config) =
    Field { config | autocomplete = Just token }


{-| What kind of on-screen keyboard a text field wants — `"numeric"`,
`"decimal"`, `"email"`, `"url"`.

There is deliberately no `type="number"`. A number input turns a scroll over it
into a changed value, accepts `e`, and reports an empty string for anything it
cannot parse, so a half-typed value vanishes from the model instead of being
reported as wrong. `inputmode` gets the keyboard without any of that, and the
validation stays where the error message is.

-}
withInputMode : String -> Field msg -> Field msg
withInputMode mode (Field config) =
    Field { config | inputMode = Just mode }


{-| Told when focus arrives in the control.

For form libraries that track focus themselves — `enbala/elm-form` keeps which
field has it, and a typehint list is shown only while it does. On a date,
markdown or code field it is focus arriving anywhere inside the editor, which is
what "the field has focus" means there.

-}
withOnFocus : msg -> Field msg -> Field msg
withOnFocus msg (Field config) =
    Field { config | onFocus = Just msg }


{-| Told when focus leaves the control.

This is what lets an error wait until someone has finished typing: `enbala/elm-form`
holds back a text field's error while it is being edited and releases it on
blur. Without it, an error could only ever appear after a submit.

-}
withOnBlur : msg -> Field msg -> Field msg
withOnBlur msg (Field config) =
    Field { config | onBlur = Just msg }


{-| Where the caret was when focus left the control.

For a field something else writes into — a template with a row of values under
it, each of which inserts itself. Without this the insertion can only go at the
end, which is wrong as soon as the template is being edited rather than
written. Read on blur because that is when focus moves to the thing doing the
inserting.

-}
withSelectionOnBlur : (( Int, Int ) -> msg) -> Field msg -> Field msg
withSelectionOnBlur toMsg (Field config) =
    Field { config | onSelectionBlur = Just toMsg }


{-| Values worth offering in a text field, which it does not insist on — an
affiliation, where most people belong to one of a handful but anyone may type
their own.

A list of the caller's own under the field, drawn by `ui-suggest`
(`packages/elm-ui/js/suggest-input.js`): it filters as the value is typed, the arrow
keys move through it, Enter takes what they are on and Escape closes it. A
`datalist` was what this used to be, and it is a different box in every browser,
unstyleable in all of them, and reachable from the keyboard in none of the ways
the rest of the system is.

**Suggestions do not restrict the value.** Whatever is typed is what the field
holds, and a value that is not among them is as valid as one that is. Where the
value must be one of a set, use `select` or `Ui.Combobox`.

Only a text `input` takes them; on any other field this does nothing.

-}
withSuggestions : List String -> Field msg -> Field msg
withSuggestions suggestions (Field config) =
    Field { config | suggestions = suggestions }


{-| Adds the strength meter to a password field, and says the strength in words
as well as in bars.
-}
withStrength : Field msg -> Field msg
withStrength (Field config) =
    Field { config | strength = Local }


{-| A strength from somewhere else — an API, usually. `strengthChecking` while
the answer is in flight and `strengthUnavailable` when it does not arrive: a
meter that shows nothing in either case reads as "your password is fine". All
three take the number of bars, so the meter does not change length when the
answer arrives.
-}
withStrengthOf : Strength -> Field msg -> Field msg
withStrengthOf strength (Field config) =
    Field { config | strength = Supplied strength }


{-| A score somebody else produced. The admin center's API answers 0 to 4; a
level of 4 out of 4 fills the bar exactly as 5 out of 5 would.
-}
strengthScored : { level : Int, max : Int, label : String } -> Strength
strengthScored score =
    Scored { level = score.level, max = score.max, label = score.label, tone = Nothing }


{-| `strengthScored`, in the tone given rather than the one its fraction would
land in: for a policy that accepts only the top of the scale, where four bars
of five are still not enough.
-}
strengthScoredAs : StrengthTone -> { level : Int, max : Int, label : String } -> Strength
strengthScoredAs tone score =
    Scored { level = score.level, max = score.max, label = score.label, tone = Just tone }


strengthChecking : { max : Int } -> Strength
strengthChecking =
    Checking


strengthUnavailable : { max : Int, label : String } -> Strength
strengthUnavailable =
    Unavailable


{-| The field's own words in the language of the page. See `Words`.
-}
withWords : Words -> Field msg -> Field msg
withWords words (Field config) =
    Field { config | words = words }


{-| Keeps the label for assistive technology and takes it off the screen.

For the one case where the field's purpose is already obvious from where it is
and what it says — a search box at the top of a listing, whose placeholder
repeats the label word for word. Two identical strings stacked on top of each
other is not twice the help.

It is a narrow exception, not a style: a placeholder disappears as soon as
someone types, so a field that needs a label to be understood needs a visible
one. Everything with a value worth checking afterwards keeps its label.

-}
withHiddenLabel : Field msg -> Field msg
withHiddenLabel (Field config) =
    Field { config | hiddenLabel = True }


{-| One control on a choice group's name's line — selecting all of a set of
tags, or none of them.

It acts on the whole set rather than on any option in it, so it belongs where
the set is named rather than after the last option, which is the end of the
reading rather than the top of it. `withGroupHeaderAction` is the same thing for
a `group`.

Only a group of choices has a line of its own to put it on; on any other field
the label belongs to the control under it and nothing else may come between
them.

-}
withLabelAction : Html msg -> Field msg -> Field msg
withLabelAction action (Field config) =
    Field { config | labelAction = Just action }


toHtml : Field msg -> Html msg
toHtml (Field config) =
    let
        common =
            [ id config.id
            , Attr.disabled config.disabled
            , Attr.required config.required
            ]
                ++ (if String.isEmpty (describedBy config) then
                        []

                    else
                        [ attribute "aria-describedby" (describedBy config) ]
                   )
                ++ (case config.error of
                        Just _ ->
                            [ attribute "aria-invalid" "true" ]

                        Nothing ->
                            []
                   )
                ++ (case config.placeholder of
                        Just placeholder ->
                            [ Attr.placeholder placeholder ]

                        Nothing ->
                            []
                   )
                ++ (case config.autocomplete of
                        Just token ->
                            [ Attr.attribute "autocomplete" token ]

                        Nothing ->
                            []
                   )
                ++ focusHandlers onFocus onBlur config
                ++ (case config.onSubmitShortcut of
                        Just onSubmit ->
                            [ onSubmitKey config.submitOnEnter onSubmit ]

                        Nothing ->
                            []
                   )

        -- `.ui-input` carries the geometry of a text field — full width, a
        -- minimum height, padding. A switch is a track with a knob in it and
        -- takes none of that: wearing both classes it was sized as a 16px
        -- square while its knob moved 16px, which is what made it look broken.
        shared =
            classList
                [ ( "ui-input", True )
                , ( "is-invalid", Maybe.isJust config.error )
                ]
                :: common

        body =
            [ viewFramed config (viewAffixed config shared)
            , case config.hint of
                Just hint ->
                    p [ class "ui-field__hint", id (hintId config) ] hint

                Nothing ->
                    text ""
            , case config.error of
                Just error ->
                    -- Color is never the only signal: the message and its icon
                    -- carry the same information (WCAG 1.4.1).
                    p [ class "ui-field__error", id (errorId config) ]
                        [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                        , text error
                        ]

                Nothing ->
                    text ""
            , case config.warning of
                Just warning ->
                    p [ class "ui-field__warning", id (warningId config) ]
                        [ span [ class "ui-field__warning-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                        , text warning
                        ]

                Nothing ->
                    text ""
            ]
    in
    case config.control of
        -- A group of choices is a group, not a labeled control: the name of the
        -- set is a legend, and `for` has nothing to point at because there are
        -- several inputs rather than one.
        Radios _ _ _ ->
            viewGroup config body

        Checks _ _ _ ->
            viewGroup config body

        -- The label belongs beside these two, not above them: what is being
        -- named is one box rather than a field with a value in it.
        Check checked onCheckMsg ->
            div [ class "ui-field" ]
                (Html.label [ class "ui-field__check", for config.id ]
                    [ Html.input
                        (type_ "checkbox"
                            :: Attr.checked checked
                            :: onCheck onCheckMsg
                            :: shared
                        )
                        []
                    , span [ class "ui-field__check-label" ] (checkLabel config ++ viewLabelBadge config)
                    ]
                    :: List.drop 1 body
                )

        Switch _ _ ->
            div [ class "ui-field" ]
                (Html.label [ class "ui-field__check ui-field__check--switch", for config.id ]
                    [ viewControl config common
                    , span [ class "ui-field__check-label" ] (checkLabel config ++ viewLabelBadge config)
                    ]
                    :: List.drop 1 body
                )

        _ ->
            div [ class "ui-field" ] (viewLabel config :: body)


checkLabel : Config msg -> List (Html msg)
checkLabel config =
    Maybe.withDefault [ text config.label ] config.labelHtml


viewLabel : Config msg -> Html msg
viewLabel config =
    label
        [ classList
            [ ( "ui-field__label", True )
            , ( "ui-visually-hidden", config.hiddenLabel )
            ]
        , id (labelId config)
        , for config.id
        ]
        (text config.label
            :: (if config.required then
                    span [ class "ui-field__required", attribute "aria-hidden" "true" ] [ text "*" ]

                else
                    text ""
               )
            :: viewLabelBadge config
        )


viewLabelBadge : Config msg -> List (Html msg)
viewLabelBadge config =
    case config.labelBadge of
        Just badge ->
            [ span [ class "ui-field__label-badge" ] [ Badge.toHtml badge ] ]

        Nothing ->
            []



-- GROUPS OF FIELDS


{-| Several fields that make one answer: a version's major, minor and patch; a
range's from and to.
-}
type FieldGroup msg
    = FieldGroup
        { id : String
        , legend : String
        , fields : List (Html msg)
        , hint : Maybe (List (Html msg))
        , error : Maybe String
        , actions : List (Html msg)
        , headerAction : Maybe (Html msg)
        , stacked : Bool
        }


{-| A `fieldset` named by a `legend`, with the fields side by side while they fit.

The group's name is said once, as focus enters the set, so each field inside
can have a short label of its own — "Major", "Minor", "Patch" — rather than
repeating the group's in every one ("New version: major"). Keep those labels
visible: a number field whose name only a screen reader hears is a box a sighted
reader has to guess at.

The group's hint and its error are about the whole answer, so they describe the
`fieldset` (`aria-describedby`) and are read as the group is entered. An error
about one of the fields belongs on that field, with `withError`.

The `id` is the fieldset's, for a tour or a link to point at, and the hint's and
the error's ids are made from it.

-}
group : { id : String, legend : String, fields : List (Html msg) } -> FieldGroup msg
group config =
    FieldGroup
        { id = config.id
        , legend = config.legend
        , fields = config.fields
        , hint = Nothing
        , error = Nothing
        , actions = []
        , headerAction = Nothing
        , stacked = False
        }


{-| One control on the group's own line, at the end of it — importing a
request from a cURL command, beside the legend that says "Request".

It acts on the group as a whole rather than on any field in it, and it belongs
where the group is named rather than after the last field, which is the end of
the reading rather than the top of it.

-}
withGroupHeaderAction : Html msg -> FieldGroup msg -> FieldGroup msg
withGroupHeaderAction action (FieldGroup config) =
    FieldGroup { config | headerAction = Just action }


{-| The fields one under another at their full width, rather than side by side:
for a set of settings rather than one answer in parts — a role's permissions in
a group, each a switch with a sentence about what it allows.
-}
withGroupStacked : FieldGroup msg -> FieldGroup msg
withGroupStacked (FieldGroup config) =
    FieldGroup { config | stacked = True }


{-| Controls that act on the whole group, under its fields and inside the
`fieldset` — the suggested next versions that fill in all three numbers at once.
Inside, so they are reached as part of the group they change.
-}
withGroupActions : List (Html msg) -> FieldGroup msg -> FieldGroup msg
withGroupActions actions (FieldGroup config) =
    FieldGroup { config | actions = actions }


withGroupHint : String -> FieldGroup msg -> FieldGroup msg
withGroupHint hint (FieldGroup config) =
    FieldGroup { config | hint = Just [ text hint ] }


{-| `withGroupHint`, for a description with something in it that plain text
cannot say — a name in code, an example. The same as `withHintHtml` is to
`withHint`.
-}
withGroupHintHtml : List (Html msg) -> FieldGroup msg -> FieldGroup msg
withGroupHintHtml hint (FieldGroup config) =
    FieldGroup { config | hint = Just hint }


withGroupError : Maybe String -> FieldGroup msg -> FieldGroup msg
withGroupError error (FieldGroup config) =
    FieldGroup { config | error = error }


groupToHtml : FieldGroup msg -> Html msg
groupToHtml (FieldGroup config) =
    let
        hintElementId =
            config.id ++ "-hint"

        errorElementId =
            config.id ++ "-error"

        describedByIds =
            [ Maybe.map (always hintElementId) config.hint
            , Maybe.map (always errorElementId) config.error
            ]
                |> Maybe.values
                |> String.join " "
    in
    fieldset
        (classList
            [ ( "ui-field", True )
            , ( "ui-field--group", True )
            , ( "is-invalid", Maybe.isJust config.error )
            ]
            :: id config.id
            -- Named by its legend explicitly. A group with a control on its
            -- name's line needs the legend in a row with it, and a `legend`
            -- names its `fieldset` only as the fieldset's first child — so the
            -- name is said here rather than left to where the element sits.
            :: attribute "aria-labelledby" (config.id ++ "-label")
            :: (if String.isEmpty describedByIds then
                    []

                else
                    [ attribute "aria-describedby" describedByIds ]
               )
        )
        [ case config.headerAction of
            Just action ->
                -- The legend and the action share a row. The legend keeps its
                -- own element, because a `legend` is what names a `fieldset`
                -- and nothing else will do.
                div [ class "ui-field__group-header" ]
                    [ legend [ class "ui-field__legend", id (config.id ++ "-label") ] [ text config.legend ]
                    , action
                    ]

            Nothing ->
                legend [ class "ui-field__legend", id (config.id ++ "-label") ] [ text config.legend ]
        , div
            [ classList
                [ ( "ui-field__group-fields", True )
                , ( "ui-field__group-fields--stacked", config.stacked )
                ]
            ]
            config.fields
        , if List.isEmpty config.actions then
            text ""

          else
            div [ class "ui-inline" ] config.actions
        , case config.hint of
            Just hint ->
                p [ class "ui-field__hint", id hintElementId ] hint

            Nothing ->
                text ""
        , case config.error of
            Just error ->
                p [ class "ui-field__error", id errorElementId ]
                    [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                    , text error
                    ]

            Nothing ->
                text ""
        ]


{-| A `fieldset` with a `legend`, which is the only markup that names a set of
inputs rather than one of them. Drawn as a field — its name as the label, the
options under it — not as a box: a group of choices is one setting with several
answers, and the box is for settings that belong together (`group`,
`Ui.Form.section`).
-}
viewGroup : Config msg -> List (Html msg) -> Html msg
viewGroup config body =
    -- The id is the group's, so a tour or a link can point at the set, and the
    -- hint and the error describe the fieldset: there is no one input to hang
    -- them on.
    let
        legendHtml =
            legend
                [ classList
                    [ ( "ui-field__legend", True )
                    , ( "ui-visually-hidden", config.hiddenLabel )
                    ]
                , id (labelId config)
                ]
                [ text config.label
                , if config.required then
                    span [ class "ui-field__required", attribute "aria-hidden" "true" ] [ text "*" ]

                  else
                    text ""
                ]

        header =
            case config.labelAction of
                Just action ->
                    div [ class "ui-field__label-row" ] [ legendHtml, action ]

                Nothing ->
                    legendHtml
    in
    fieldset
        (List.concat
            [ [ class "ui-field ui-field--group ui-field--choices", id config.id ]

            -- A `legend` names its `fieldset` only as the fieldset's first
            -- child. With a control beside it the legend is inside a row
            -- instead, so the name is said here rather than left to where the
            -- element sits.
            , case config.labelAction of
                Just _ ->
                    [ attribute "aria-labelledby" (labelId config) ]

                Nothing ->
                    []
            , if String.isEmpty (describedBy config) then
                []

              else
                [ attribute "aria-describedby" (describedBy config) ]
            ]
        )
        (header :: body)


{-| The control, with whatever sits in front of or after it.

The affixes are described rather than decorative: a field whose suffix says
"hours" means something different from the same field without it, so the suffix
is in `aria-describedby` and not just on the screen.

-}
viewFramed : Config msg -> Html msg -> Html msg
viewFramed config control =
    case config.frame of
        Nothing ->
            control

        Just frame ->
            let
                bar modifier items =
                    if List.isEmpty items then
                        text ""

                    else
                        div [ class ("ui-field__frame-bar " ++ modifier) ] items
            in
            div [ class "ui-field__frame" ]
                [ bar "ui-field__frame-bar--start" frame.header
                , case frame.instead of
                    Just instead ->
                        div [ class "ui-field__frame-instead" ] [ instead, div [ Attr.hidden True ] [ control ] ]

                    Nothing ->
                        control
                , bar "ui-field__frame-bar--end" frame.footer
                ]


viewAffixed : Config msg -> List (Html.Attribute msg) -> Html msg
viewAffixed config shared =
    case ( config.prefix, config.suffix, config.suffixIcon ) of
        ( Nothing, Nothing, Nothing ) ->
            viewControl config shared

        _ ->
            div [ class "ui-field__affixed" ]
                (Maybe.values
                    [ Maybe.map
                        (\prefix -> span [ class "ui-field__affix", id (prefixId config) ] [ text prefix ])
                        config.prefix
                    , Just (viewControl config shared)
                    , Maybe.map
                        (\suffix -> span [ class "ui-field__affix", id (suffixId config) ] [ text suffix ])
                        config.suffix
                    , Maybe.map
                        (\icon ->
                            span [ class "ui-field__affix ui-field__affix--icon", attribute "aria-hidden" "true" ]
                                [ Icon.toHtml icon ]
                        )
                        config.suffixIcon
                    ]
                )


{-| The select's shape, as a button that does nothing — `withDisabledReason`.

Named by the label and by its own text, the value, so it reads as "Permissions
for Ada, Owner"; described by the reason first and then by whatever else the
field says. No `disabled`: that is what would take it out of the tab order.

-}
viewUnavailableSelect : Config msg -> { id : String, reason : String } -> List ( String, String ) -> String -> Html msg
viewUnavailableSelect config reason options current =
    let
        currentLabel =
            options
                |> List.find (\( optionValue, _ ) -> optionValue == current)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault current
    in
    Tooltip.viewDescribing { id = reason.id, label = reason.reason, placement = Tooltip.End }
        (span [ class "ui-select ui-select--unavailable" ]
            [ Html.button
                [ type_ "button"
                , id config.id
                , class "ui-input"
                , attribute "aria-disabled" "true"
                , attribute "aria-labelledby" (labelId config ++ " " ++ config.id)
                , attribute "aria-describedby" (String.join " " (List.filterNot String.isEmpty [ reason.id, describedBy config ]))
                ]
                [ text currentLabel ]
            ]
        )


{-| The ids of whatever describes the control — the hint, the error, or both.
Built from the field's own id, so nothing has to be passed in and nothing can be
forgotten.
-}
describedBy : Config msg -> String
describedBy config =
    [ Maybe.map (always (prefixId config)) config.prefix
    , Maybe.map (always (suffixId config)) config.suffix
    , Maybe.map (always (hintId config)) config.hint
    , Maybe.map (always (errorId config)) config.error
    , Maybe.map (always (warningId config)) config.warning
    , if config.strength == NoStrength then
        Nothing

      else
        Just (strengthId config)
    ]
        |> Maybe.values
        |> String.join " "


labelId : Config msg -> String
labelId config =
    config.id ++ "-label"


hintId : Config msg -> String
hintId config =
    config.id ++ "-hint"


errorId : Config msg -> String
errorId config =
    config.id ++ "-error"


warningId : Config msg -> String
warningId config =
    config.id ++ "-warning"


viewControl : Config msg -> List (Html.Attribute msg) -> Html msg
viewControl config shared =
    case config.control of
        Text current onInputMsg ->
            let
                field =
                    Html.input
                        (type_ "text"
                            :: value current
                            :: onInput onInputMsg
                            :: Maybe.values [ Maybe.map (attribute "inputmode") config.inputMode ]
                            ++ shared
                        )
                        []
            in
            if List.isEmpty config.suggestions then
                field

            else
                -- The element wraps the input rather than replacing it: the id,
                -- the label, the error and the describedby are the field's, and
                -- what it adds is the list under them. It sets a value by
                -- sending the same `input` event typing sends.
                Html.node "ui-suggest"
                    [ class "ui-suggest"
                    , attribute "suggestions" (Encode.encode 0 (Encode.list Encode.string config.suggestions))
                    , attribute "label" config.label
                    ]
                    [ field ]

        Multiline current onInputMsg ->
            Html.textarea
                (value current
                    :: onInput onInputMsg
                    :: class "ui-input--multiline"
                    :: (case config.rows of
                            Just count ->
                                [ Attr.rows count, class "ui-input--rows" ]

                            Nothing ->
                                []
                       )
                    ++ shared
                )
                []

        Options options current onInputMsg ->
            case config.disabledReason of
                Just reason ->
                    viewUnavailableSelect config reason options current

                Nothing ->
                    -- Wrapped for the chevron, which the stylesheet draws: the
                    -- native arrow sits wherever each browser puts it, hard
                    -- against the edge in some, and a select cannot hold a
                    -- pseudo-element of its own.
                    span [ class "ui-select" ]
                        [ Html.select (onInput onInputMsg :: shared)
                            (List.map
                                (\( optionValue, optionLabel ) ->
                                    option [ value optionValue, selected (optionValue == current) ] [ text optionLabel ]
                                )
                                options
                            )
                        ]

        GroupedOptions loose groups current onInputMsg ->
            let
                viewOption ( optionValue, optionLabel ) =
                    option [ value optionValue, selected (optionValue == current) ] [ text optionLabel ]
            in
            span [ class "ui-select" ]
                [ Html.select (onInput onInputMsg :: shared)
                    (List.map viewOption loose
                        ++ List.map
                            (\( groupLabel, groupOptions ) ->
                                Html.optgroup [ attribute "label" groupLabel ] (List.map viewOption groupOptions)
                            )
                            groups
                    )
                ]

        Switch checked onCheckMsg ->
            -- role="switch" on a real checkbox: announced as on or off rather
            -- than checked, and still a checkbox to anything that does not know
            -- the role.
            Html.input
                (type_ "checkbox"
                    :: attribute "role" "switch"
                    :: Attr.checked checked
                    :: onCheck onCheckMsg
                    :: classList
                        [ ( "ui-switch", True )
                        , ( "is-invalid", Maybe.isJust config.error )
                        ]
                    :: shared
                )
                []

        Password current onInputMsg ->
            div [ class "ui-field__stack" ]
                (Html.input
                    (type_ "password"
                        :: value current
                        :: onInput onInputMsg
                        :: shared
                    )
                    []
                    :: (case meterFor config current of
                            Just meter ->
                                [ viewStrength config meter ]

                            Nothing ->
                                []
                       )
                )

        Secret entry ->
            div [ class "ui-field__affixed" ]
                [ Html.input
                    (type_
                        (if entry.revealed then
                            "text"

                         else
                            "password"
                        )
                        :: value entry.value
                        :: onInput entry.onInput
                        :: shared
                    )
                    []
                , Html.button
                    [ type_ "button"
                    , class "ui-field__reveal"
                    , Attr.disabled config.disabled

                    -- Pressed, not two buttons: it is one control whose state is
                    -- whether the value is showing.
                    , attribute "aria-pressed" (boolToString entry.revealed)
                    , attribute "aria-label" (config.words.show config.label)
                    , onClick (entry.onReveal (not entry.revealed))
                    ]
                    [ Icon.toHtml
                        (Icon.solid
                            (if entry.revealed then
                                "fa-eye-slash"

                             else
                                "fa-eye"
                            )
                        )
                    ]
                ]

        Swatch current onInputMsg ->
            let
                -- Empty, the hex shows the placeholder, and the picker the same
                -- color rather than a black that is not the value either.
                pickerCurrent =
                    if String.isEmpty (String.trim current) then
                        Maybe.withDefault current config.placeholder

                    else
                        current
            in
            div [ class "ui-field__stack ui-field__stack--row" ]
                [ div [ class "ui-color" ]
                    [ -- The platform's own picker: a wheel, a spectrum, an
                      -- eyedropper on some systems, and none of it ours to
                      -- rebuild. It is a real control, so it is keyboard
                      -- reachable and the value it produces is always a valid
                      -- hex.
                      Html.input
                        [ type_ "color"
                        , value (pickerValue pickerCurrent)
                        , onInput onInputMsg
                        , Attr.disabled config.disabled
                        , class "ui-color__picker"

                        -- Its own name: the field's label belongs to the text
                        -- input, which is the one that spells the value out.
                        , attribute "aria-label" (config.words.pick config.label)
                        ]
                        []
                    , div [ class "ui-color__hex" ]
                        [ Html.input
                            (type_ "text"
                                :: value current
                                :: onInput onInputMsg
                                :: classList [ ( "ui-field__hex", True ), ( "ui-field__hex--clearable", config.clearable ) ]
                                :: shared
                            )
                            []
                        , if config.clearable && not (String.isEmpty current) then
                            Html.button
                                [ type_ "button"
                                , class "ui-color__clear"
                                , Attr.disabled config.disabled
                                , attribute "aria-label" (config.words.clear config.label)
                                , onClick (onInputMsg "")
                                ]
                                [ Icon.toHtml (Icon.solid "fa-xmark") ]

                          else
                            text ""
                        ]
                    ]
                , if List.isEmpty config.swatches then
                    text ""

                  else
                    div
                        [ class "ui-swatches"
                        , attribute "role" "radiogroup"
                        , attribute "aria-labelledby" (labelId config)
                        ]
                        (List.map (viewSwatch config current onInputMsg) config.swatches)
                ]

        Static content ->
            -- Not an input, so not a tab stop and not something to fill in. The
            -- id goes here because the label still points at it, and the
            -- description is named here too: a value that is only read still
            -- has one, and a hint nothing points at is never announced.
            p
                (class "ui-field__static"
                    :: id config.id
                    :: (if String.isEmpty (describedBy config) then
                            []

                        else
                            [ attribute "aria-describedby" (describedBy config) ]
                       )
                )
                content

        Radios options current onChangeMsg ->
            div (choicesAttributes config (List.length options))
                (List.map (viewRadio config current onChangeMsg) options)

        Checks options values onToggleMsg ->
            div (choicesAttributes config (List.length options))
                (List.map (viewCheck config values onToggleMsg) options)

        Check _ _ ->
            text ""

        Markup current onChangeMsg ->
            viewMarkup config current onChangeMsg

        Code language current onChangeMsg ->
            Html.node "ui-code-editor"
                ([ class "ui-code-editor"
                 , attribute "language" language
                 , attribute "input-id" config.id

                 -- A CodeMirror editor is a contenteditable with a role and no
                 -- name. `for` cannot reach it either, so the label is pointed
                 -- at rather than pointing.
                 , attribute "labelledby" (labelId config)
                 , Attr.property "editorValue" (Encode.string current)
                 , on "editorChanged"
                    (Decode.map onChangeMsg (Decode.at [ "target", "editorValue" ] Decode.string))
                 ]
                    ++ presentIf (not config.codeWrap) (attribute "wrap" "off")
                    ++ (if String.isEmpty (describedBy config) then
                            []

                        else
                            [ attribute "describedby" (describedBy config) ]
                       )
                    ++ presentIf (Maybe.isJust config.error) (attribute "invalid" "")
                    ++ presentIf (Maybe.isJust config.error) (class "is-invalid")
                    ++ presentIf config.disabled (attribute "disabled" "")
                    ++ presentIf config.disabled (class "is-disabled")
                    ++ elementFocusHandlers config
                )
                []

        Moment kind current onChangeMsg ->
            -- The element owns its input, so everything the field would have put
            -- on one goes across as an attribute for it to apply: flatpickr
            -- replaces the input outright when it shows an alternate format, and
            -- the replacement carries none of the original's identity.
            Html.node "ui-date-picker"
                ([ attribute "mode" (kindToMode kind)
                 , attribute "input-id" config.id
                 , attribute "label" (String.toLower config.label)
                 , attribute "words"
                    (Encode.encode 0
                        (Encode.object
                            [ ( "clear", Encode.string (config.words.clear config.label) )
                            , ( "invalidValue", Encode.string (config.words.invalidDate "{value}") )
                            ]
                        )
                    )
                 , Attr.property "pickerValue" (Encode.string current)
                 , on "uiDatePickerChanged"
                    (Decode.map onChangeMsg (Decode.at [ "target", "pickerValue" ] Decode.string))
                 ]
                    ++ (if String.isEmpty (describedBy config) then
                            []

                        else
                            [ attribute "describedby" (describedBy config) ]
                       )
                    ++ presentIf (Maybe.isJust config.error) (attribute "invalid" "")
                    ++ presentIf config.disabled (attribute "disabled" "")
                    ++ presentIf config.required (attribute "required" "")
                    ++ elementFocusHandlers config
                    ++ (case config.placeholder of
                            Just placeholder ->
                                [ attribute "placeholder" placeholder ]

                            Nothing ->
                                []
                       )
                )
                []


viewMarkup : Config msg -> String -> (String -> msg) -> Html msg
viewMarkup config current onChangeMsg =
    Html.node "ui-markdown-editor"
        ([ attribute "input-id" config.id

         -- The editing area is a contenteditable div, which `for` cannot point
         -- at — `for` only reaches form controls. The label is named instead.
         , attribute "labelledby" (labelId config)
         , attribute "label" config.label
         , attribute "words" (Encode.encode 0 (markdownWords config))
         , Attr.property "editorValue" (Encode.string current)
         , on "editorChanged"
            (Decode.map onChangeMsg (Decode.at [ "target", "editorValue" ] Decode.string))
         ]
            ++ (if String.isEmpty (describedBy config) then
                    []

                else
                    [ attribute "describedby" (describedBy config) ]
               )
            ++ presentIf (Maybe.isJust config.error) (attribute "invalid" "")
            ++ presentIf config.disabled (attribute "disabled" "")
            ++ presentIf config.required (attribute "required" "")
            ++ elementFocusHandlers config
        )
        []


{-| The toolbar's words, keyed the way `packages/elm-ui/js/markdown-editor.js` reads
them.
-}
markdownWords : Config msg -> Encode.Value
markdownWords config =
    let
        words =
            config.words.markdown
    in
    Encode.object
        (List.map (Tuple.mapSecond Encode.string)
            [ ( "heading2", words.heading2 )
            , ( "heading3", words.heading3 )
            , ( "bold", words.bold )
            , ( "italic", words.italic )
            , ( "strike", words.strikethrough )
            , ( "link", words.link )
            , ( "image", words.image )
            , ( "bulletList", words.bulletList )
            , ( "orderedList", words.numberedList )
            , ( "code", words.inlineCode )
            , ( "codeBlock", words.codeBlock )
            , ( "quote", words.quote )
            , ( "linkPrompt", words.linkAddress )
            , ( "imagePrompt", words.imageAddress )
            , ( "apply", words.apply )
            , ( "cancel", words.cancel )
            , ( "modes", words.editingMode )
            , ( "rich", words.formatted )
            , ( "markdown", words.markdown )
            , ( "toolbar", words.toolbar config.label )
            ]
        )


kindToMode : Kind -> String
kindToMode kind =
    case kind of
        Day ->
            "date"

        DayTime ->
            "datetime"

        Clock ->
            "time"


{-| The focus hooks on a native control.
-}
focusHandlers : (msg -> Html.Attribute msg) -> (msg -> Html.Attribute msg) -> Config msg -> List (Html.Attribute msg)
focusHandlers toFocus toBlur config =
    Maybe.values
        [ Maybe.map toFocus config.onFocus
        , Maybe.map toBlur config.onBlur
        , Maybe.map (\toSelection -> on "blur" (selectionDecoder toSelection)) config.onSelectionBlur
        ]


{-| The caret's place in the control the event came from. A control with no
selection to report — anything that is not a text box — has neither property,
and the handler simply does not fire.
-}
selectionDecoder : (( Int, Int ) -> msg) -> Decode.Decoder msg
selectionDecoder toMsg =
    Decode.map2 (\start end -> toMsg ( start, end ))
        (Decode.at [ "target", "selectionStart" ] Decode.int)
        (Decode.at [ "target", "selectionEnd" ] Decode.int)


{-| The same hooks on a custom element, which owns its input — so they listen
for focus moving within it, which bubbles, rather than on the element itself,
which is never focused.
-}
elementFocusHandlers : Config msg -> List (Html.Attribute msg)
elementFocusHandlers =
    focusHandlers (on "focusin" << Decode.succeed) (on "focusout" << Decode.succeed)


presentIf : Bool -> Html.Attribute msg -> List (Html.Attribute msg)
presentIf condition attr =
    if condition then
        [ attr ]

    else
        []


{-| What the meter shows, whoever worked it out.

`Nothing` when there is nothing to say — no strength asked for, or an empty
field. A meter that shows an empty bar for an empty password reads as a verdict
on it.

-}
meterFor : Config msg -> String -> Maybe Meter
meterFor config current =
    case config.strength of
        NoStrength ->
            Nothing

        Local ->
            if String.isEmpty current then
                Nothing

            else
                let
                    ( level, name ) =
                        strengthOf config.words current
                in
                Just { level = level, max = 5, label = name, busy = False, failed = False, tone = Nothing }

        Supplied (Checking checking) ->
            Just { level = 0, max = max 1 checking.max, label = config.words.checking, busy = True, failed = False, tone = Nothing }

        Supplied (Scored score) ->
            Just
                { level = score.level
                , max = max 1 score.max
                , label = score.label
                , busy = False
                , failed = False
                , tone = score.tone
                }

        Supplied (Unavailable unavailable) ->
            Just { level = 0, max = max 1 unavailable.max, label = unavailable.label, busy = False, failed = True, tone = Nothing }


type alias Meter =
    { level : Int
    , max : Int
    , label : String
    , busy : Bool
    , failed : Bool
    , tone : Maybe StrengthTone
    }


{-| The strength, in words and in bars.

The words are the part that matters: bars alone say nothing to a screen reader
and nothing to anyone who cannot separate red from green (1.4.1). There is an
icon as well, because three colors of bar are three colors, and a tick is a
tick.

Polite, because it changes while someone is typing and is not what they are
doing.

-}
viewStrength : Config msg -> Meter -> Html msg
viewStrength config meter =
    let
        tone =
            toneOf meter
    in
    div [ classList [ ( "ui-strength", True ), ( "is-busy", meter.busy ) ] ]
        [ div
            [ class ("ui-strength__bars is-" ++ tone)
            , attribute "aria-hidden" "true"
            ]
            (List.map
                (\step ->
                    span
                        [ classList
                            [ ( "ui-strength__bar", True )
                            , ( "is-on", not meter.busy && not meter.failed && step <= meter.level )
                            ]
                        ]
                        []
                )
                (List.range 1 meter.max)
            )
        , span
            [ class ("ui-strength__label is-" ++ tone)
            , id (strengthId config)
            , attribute "aria-live" "polite"
            ]
            [ span [ class "ui-strength__icon", attribute "aria-hidden" "true" ]
                [ Icon.toHtml
                    (if meter.busy then
                        Icon.spinner

                     else
                        Icon.solid (iconOf meter)
                    )
                ]
            , text meter.label
            ]
        ]


{-| Three buckets, the caller's where it gave one (`strengthScoredAs`), and
otherwise taken from the fraction rather than from a level, so a score out of
four and a score out of five land in the same places.
-}
toneOf : Meter -> String
toneOf meter =
    if meter.failed then
        "unknown"

    else if meter.busy then
        "busy"

    else
        case meter.tone of
            Just WeakTone ->
                "weak"

            Just FairTone ->
                "fair"

            Just StrongTone ->
                "strong"

            Nothing ->
                toneOfFraction meter


toneOfFraction : Meter -> String
toneOfFraction meter =
    let
        fraction =
            toFloat meter.level / toFloat meter.max
    in
    if fraction >= 0.8 then
        "strong"

    else if fraction >= 0.4 then
        "fair"

    else
        "weak"


iconOf : Meter -> String
iconOf meter =
    if meter.failed then
        "fa-triangle-exclamation"

    else
        case toneOf meter of
            "strong" ->
                "fa-circle-check"

            "fair" ->
                "fa-circle-exclamation"

            _ ->
                "fa-circle-xmark"


{-| What the local meter says, in words, for a given password.

Exposed because it is the part that carries the meaning: an app that reports
strength somewhere else — a sign-up summary, a password policy page — should say
the same words as the meter rather than inventing its own.

-}
strengthLabel : String -> String
strengthLabel =
    strengthLabelWithWords english


{-| `strengthLabel`, in the words given.
-}
strengthLabelWithWords : Words -> String -> String
strengthLabelWithWords words current =
    Tuple.second (strengthOf words current)


strengthOf : Words -> String -> ( Int, String )
strengthOf words current =
    if String.isEmpty current then
        ( 0, "" )

    else
        case (Rumkin.getStats current).strength of
            Rumkin.VeryWeak ->
                ( 1, words.strength.veryWeak )

            Rumkin.Weak ->
                ( 2, words.strength.weak )

            Rumkin.Reasonable ->
                ( 3, words.strength.reasonable )

            Rumkin.Strong ->
                ( 4, words.strength.strong )

            Rumkin.VeryStrong ->
                ( 5, words.strength.veryStrong )


{-| A swatch is named by its value, because a color is not a name.

Selected is a tick inside it: white, or black where white does not reach the
3:1 a graphic needs on that color (1.4.11). Not a ring around it: the ring is where focus is drawn, and a swatch that
is both has to show both.

-}
viewSwatch : Config msg -> String -> (String -> msg) -> String -> Html msg
viewSwatch config current onInputMsg swatch =
    let
        -- The same color written two ways (`#FFF`, `#ffffff`) is the same swatch.
        isSelected =
            pickerValue swatch == pickerValue current
    in
    Html.button
        [ type_ "button"
        , attribute "role" "radio"
        , attribute "aria-checked" (boolToString isSelected)
        , attribute "aria-label" swatch
        , Attr.disabled config.disabled
        , classList
            [ ( "ui-swatch", True )
            , ( "is-selected", isSelected )
            ]
        , Attr.style "background" swatch
        , onClick (onInputMsg swatch)
        ]
        [ if isSelected then
            span [ class "ui-swatch__tick", Attr.style "color" (tickColor swatch) ]
                [ Icon.toHtml (Icon.solid "fa-check") ]

          else
            text ""
        ]


tickColor : String -> String
tickColor swatch =
    let
        ground =
            hexToColor (pickerValue swatch)
    in
    if Oklch.contrast ground Color.white >= 3 then
        "#ffffff"

    else
        "#000000"


{-| `#rrggbb`, as `pickerValue` writes it, to a color.
-}
hexToColor : String -> Color
hexToColor hex =
    let
        channel from =
            String.slice from (from + 2) hex
                |> String.toList
                |> List.foldl (\c sum -> sum * 16 + hexDigit c) 0
                |> (\value_ -> toFloat value_ / 255)
    in
    Color.rgb (channel 1) (channel 3) (channel 5)


hexDigit : Char -> Int
hexDigit c =
    if Char.isDigit c then
        Char.toCode c - Char.toCode '0'

    else
        Char.toCode (Char.toLower c) - Char.toCode 'a' + 10


{-| The options' container. Inline, it also carries how many options there are,
which is what decides the width at which they stop fitting on one row.
-}
choicesAttributes : Config msg -> Int -> List (Html.Attribute msg)
choicesAttributes config count =
    classList
        [ ( "ui-choices", True )
        , ( "ui-choices--inline", config.choicesInline )
        ]
        :: (if config.choicesInline then
                [ Attr.style "--ui-choices-count" (String.fromInt count) ]

            else
                []
           )


viewRadio : Config msg -> String -> (String -> msg) -> Choice msg -> Html msg
viewRadio config current onChangeMsg (Choice entry) =
    viewChoice config
        { inputType = "radio"
        , checked = entry.value == current
        , onPick = onChangeMsg entry.value
        , entry = entry
        }


viewCheck : Config msg -> List String -> (String -> msg) -> Choice msg -> Html msg
viewCheck config values onToggleMsg (Choice entry) =
    viewChoice config
        { inputType = "checkbox"
        , checked = List.member entry.value values
        , onPick = onToggleMsg entry.value
        , entry = entry
        }


{-| One option, with everything about it inside its own `label` — the box, the
name and the sentence explaining it. A description outside the label is a
sentence a screen reader reads separately from the thing it describes.
-}
viewChoice :
    Config msg
    ->
        { inputType : String
        , checked : Bool
        , onPick : msg
        , entry :
            { value : String
            , label : String
            , description : Maybe String
            , icon : Maybe Icon
            , color : Maybe String
            , badge : Maybe String
            , tone : Maybe ChoiceTone
            }
        }
    -> Html msg
viewChoice config option =
    label
        [ classList
            [ ( "ui-choice", True )
            , ( "is-selected", option.checked )
            , ( "ui-choice--info", option.entry.tone == Just InfoTone )
            , ( "ui-choice--warning", option.entry.tone == Just WarningTone )
            , ( "ui-choice--danger", option.entry.tone == Just DangerTone )
            ]

        -- The group and the value, never the words: a test picks an option
        -- by what it sets.
        , TestId.attribute (config.id ++ "_" ++ option.entry.value)
        ]
        [ Html.input
            ([ type_ option.inputType
             , Attr.name config.id
             , value option.entry.value
             , Attr.checked option.checked
             , Attr.disabled config.disabled
             , class "ui-choice__input"

             -- onCheck rather than onClick: a radio is also reached with the
             -- arrow keys, which fires change and not click.
             , onCheck (always option.onPick)
             ]
                ++ focusHandlers onFocus onBlur config
            )
            []
        , span [ class "ui-choice__body" ]
            (Maybe.values
                [ Just
                    (span [ class "ui-choice__label" ]
                        (Maybe.values
                            [ Maybe.map
                                (\colorValue ->
                                    span
                                        [ class "ui-choice__color"
                                        , attribute "aria-hidden" "true"
                                        , Attr.style "background" colorValue
                                        ]
                                        []
                                )
                                option.entry.color
                            , Maybe.map (\icon -> span [ class "ui-choice__icon" ] [ Icon.toHtml icon ]) option.entry.icon
                            , Just (text option.entry.label)
                            , Maybe.map (\badgeLabel -> span [ class "ui-badge" ] [ text badgeLabel ]) option.entry.badge
                            ]
                        )
                    )
                , Maybe.map (\description -> span [ class "ui-choice__description" ] [ text description ]) option.entry.description
                ]
            )
        ]


{-| What the native picker is given: `input type="color"` only understands
`#rrggbb`, and shows anything else as black — then reports black back as the
value on the first interaction.

So the value is normalized for the picker, not only checked: `#fff`, `FFF`,
`#FFFFFF`, and the hex with an alpha channel (`#rrggbbaa`, `#rgba`, whose alpha
the picker cannot show) all become the six lowercase digits the picker takes.
Before, only an exact seven-character `#rrggbb` was passed through, so a value
typed or configured as `#fff` — the default text color of an external login
button — left the picker showing black. Anything that is not a color yet, such
as half-typed hex, still shows black, and stays as typed in the text input.

-}
pickerValue : String -> String
pickerValue current =
    let
        digits =
            String.toLower (String.trim current)
                |> (\value ->
                        if String.startsWith "#" value then
                            String.dropLeft 1 value

                        else
                            value
                   )

        isHex =
            String.all Char.isHexDigit digits
    in
    if not isHex then
        "#000000"

    else
        case String.length digits of
            3 ->
                "#" ++ doubleDigits digits

            4 ->
                "#" ++ doubleDigits (String.left 3 digits)

            6 ->
                "#" ++ digits

            8 ->
                "#" ++ String.left 6 digits

            _ ->
                "#000000"


{-| `abc` → `aabbcc`, the long form of a short hex color.
-}
doubleDigits : String -> String
doubleDigits =
    String.toList >> List.concatMap (\c -> [ c, c ]) >> String.fromList


boolToString : Bool -> String
boolToString value_ =
    if value_ then
        "true"

    else
        "false"


strengthId : Config msg -> String
strengthId config =
    config.id ++ "-strength"


prefixId : Config msg -> String
prefixId config =
    config.id ++ "-prefix"


suffixId : Config msg -> String
suffixId config =
    config.id ++ "-suffix"
