module Common.Components.Form.Field exposing
    ( Field
    , VersionSuggestion
    , checkbox
    , checkboxGroup
    , code
    , color
    , date
    , dateTime
    , email
    , errorText
    , fieldId
    , fileSize
    , fileUpload
    , fileUploadWithTestId
    , hours
    , input
    , list
    , listItemPath
    , listMapped
    , listWithHint
    , listWithHintMarkdown
    , markdown
    , number
    , password
    , passwordWithStrength
    , radioGroup
    , readOnly
    , secret
    , secretRevealedPath
    , select
    , setVersion
    , textarea
    , time
    , toHtml
    , toggle
    , version
    , versionHint
    , versionSuggestions
    , versionWithSuggestions
    , withAutocomplete
    , withChoicesInline
    , withClear
    , withDisabled
    , withDisabledReason
    , withHiddenLabel
    , withHint
    , withHintMarkdown
    , withIdPrefix
    , withPlaceholder
    , withPrefix
    , withRequired
    , withRows
    , withSuffix
    , withSuffixIcon
    , withSuggestions
    , withSwatches
    , withWarning
    )

{-| `enbala/elm-form` fields, rendered by `Ui.Field`.

This is the whole of the adapter the field module's own documentation promises:
`Ui.Field` takes a value, a message and an error and knows nothing about where
they come from; this reads them out of a `Form`, and sends what the control does
back as a `Form.Msg`. The label, the id, the hint, the error and the
`aria-describedby` between them stay `Ui.Field`'s — nothing here builds markup of
its own, so nothing here can get them wrong.

Every constructor has the same first four arguments — the locale, the form, the
field's path and its label — so a form reads as a column of fields, and anything
a particular kind needs (the options of a select, the language of a code field)
comes last.

    Field.input appState form "email" (gettext "Email" appState.locale)
        |> Field.withRequired True
        |> Field.toHtml

Three decisions worth knowing:

  - **The error appears when elm-form says it does.** `liveError` is held back
    while a text field is being typed into and released when focus leaves it,
    or at once after a submit. That is the timing `Ui` asks for, and it
    only works because every control reports focus and blur — which is why
    `Ui.Field` has `withOnFocus` and `withOnBlur`.
  - **The id comes from the path.** `form-` and the path with its separators
    turned into hyphens, so `parameters.0.name` is `form-parameters-0-name`. It
    is stable across renders, unique within a form by construction, and it is
    what the first-error focus in `Common.Components.Form` lands on.
  - **The message is the one the wizard already shows.** It comes from
    `Common.Utils.Form.fieldErrorToString`, so the wording and the msgids are
    unchanged, including the one case that function exists for: an empty number
    field says it is empty, not that it is not a number.

-}

import Common.Components.Markdown as Markdown
import Common.Components.PasswordStrength as PasswordStrength
import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale)
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field
import Form.Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, text)
import List.Extra as List
import Maybe.Extra as Maybe
import Rumkin
import String.Format as String
import Ui.Button as Button exposing (Button)
import Ui.Field as UiField
import Ui.FileUpload as FileUpload
import Ui.Icon exposing (Icon)
import Ui.ListField as ListField
import Ui.Text as Text
import Version exposing (Version)


{-| A field of an elm-form form, not yet rendered.

Opaque, and always in `Form.Msg`: the error is decided here from the form, so a
caller setting one of their own would disagree with the form about whether the
field is valid. Map the rendered `Html` into the page's message as usual.

-}
type Field
    = Field (UiField.Field Form.Msg)



-- TEXT


input : WithLocale a -> Form FormError o -> String -> String -> Field
input appState form path label =
    textLike UiField.input Form.Text appState form path label


{-| An email address. A text field with the email keyboard, not
`type="email"`: the form is validated by elm-form and says so in words, and the
browser's own check would only add a second, differently worded, opinion.

It does not set `autocomplete "email"` — on a form that creates somebody else's
account that would offer the administrator's own address. Add it where the
address is the reader's.

-}
email : WithLocale a -> Form FormError o -> String -> String -> Field
email appState form path label =
    input appState form path label
        |> mapUi (UiField.withInputMode "email")


{-| A whole number. See `Ui.Field.withInputMode` for why this is not
`type="number"`.
-}
number : WithLocale a -> Form FormError o -> String -> String -> Field
number appState form path label =
    input appState form path label
        |> mapUi (UiField.withInputMode "numeric")


textarea : WithLocale a -> Form FormError o -> String -> String -> Field
textarea appState form path label =
    textLike UiField.textarea Form.Textarea appState form path label


{-| A number of bytes, with what that is in words after it: `≈ 1.5 MB`.
-}
fileSize : WithLocale a -> Form FormError o -> String -> String -> Field
fileSize appState form path label =
    number appState form path label
        |> mapUi (UiField.withSuffix ("≈ " ++ ByteUnits.toReadable (intValue form path)))


{-| A number of hours, with the same in days after it: `≈ 2.5 days`.
-}
hours : WithLocale a -> Form FormError o -> String -> String -> Field
hours appState form path label =
    let
        days =
            String.fromFloat (toFloat (round (10 * toFloat (intValue form path) / 24)) / 10)
    in
    number appState form path label
        |> mapUi (UiField.withSuffix ("≈ " ++ days ++ " " ++ gettext "days" appState.locale))



-- PASSWORDS


{-| A password nobody needs to judge: signing in, confirming the current one.
Pair it with `withAutocomplete "current-password"`.
-}
password : WithLocale a -> Form FormError o -> String -> String -> Field
password appState form path label =
    textLike UiField.password Form.Text appState form path label


{-| A new password, with the meter.

The meter is `Ui.Field`'s. Its verdict is the server's where the client has the
server judge it (`Common.Components.PasswordStrength`, which the page keeps and
feeds the password); otherwise it is worked out here, with the same library as
`Ui`'s local meter, so that its words are translated — that meter cannot know
`Gettext`. Nothing is said about an empty field. Pair it with
`withAutocomplete "new-password"`.

-}
passwordWithStrength : WithLocale a -> PasswordStrength.Model -> Form FormError o -> String -> String -> Field
passwordWithStrength appState passwordStrength form path label =
    let
        value =
            stringValue form path

        withMeter field =
            if String.isEmpty value then
                field

            else
                case PasswordStrength.toUiStrength appState passwordStrength of
                    Just checked ->
                        UiField.withStrengthOf checked field

                    Nothing ->
                        UiField.withStrengthOf (strength appState value) field
    in
    password appState form path label
        |> mapUi withMeter


strength : WithLocale a -> String -> UiField.Strength
strength appState value =
    let
        ( level, words ) =
            case (Rumkin.getStats value).strength of
                Rumkin.VeryWeak ->
                    ( 1, gettext "Very weak" appState.locale )

                Rumkin.Weak ->
                    ( 2, gettext "Weak" appState.locale )

                Rumkin.Reasonable ->
                    ( 3, gettext "Reasonable" appState.locale )

                Rumkin.Strong ->
                    ( 4, gettext "Strong" appState.locale )

                Rumkin.VeryStrong ->
                    ( 5, gettext "Very strong" appState.locale )
    in
    UiField.strengthScored { level = level, max = 5, label = words }


{-| A value that is hidden until asked for: a client secret, a token.

Whether it is showing is kept in the form itself, in a helper field next to the
value (`secretRevealedPath`). That keeps the whole field in `Form.Msg`, so a page does not need a message
and a model field per secret — and the helper's name ends in `__`, which
`Common.Utils.Form.containsChanges` ignores, so looking at a secret is not an
unsaved change.

-}
secret : WithLocale a -> Form FormError o -> String -> String -> Field
secret appState form path label =
    let
        state =
            Form.getFieldAsString path form

        revealedPath =
            secretRevealedPath path
    in
    UiField.secret
        { id = fieldId path
        , label = label
        , value = Maybe.withDefault "" state.value
        , onInput = Form.Input path Form.Text << Form.Field.String
        , revealed = Maybe.withDefault False (Form.getFieldAsBool revealedPath form).value
        , onReveal = Form.Input revealedPath Form.Checkbox << Form.Field.Bool
        }
        |> finish appState path (errorText appState label state)


{-| Where `secret` keeps whether the value is showing.
-}
secretRevealedPath : String -> String
secretRevealedPath path =
    path ++ "-visible__"



-- CHOICES


select : WithLocale a -> Form FormError o -> String -> String -> List ( String, String ) -> Field
select appState form path label options =
    let
        state =
            Form.getFieldAsString path form
    in
    UiField.select
        { id = fieldId path
        , label = label
        , value = Maybe.withDefault "" state.value
        , options = options
        , onInput = Form.Input path Form.Select << Form.Field.String
        }
        |> finish appState path (errorText appState label state)


{-| On or off, taking effect when the form is saved — a checkbox with the switch's
look and announcement. See `Ui.Field.toggle` for when that is the right shape.
-}
toggle : WithLocale a -> Form FormError o -> String -> String -> Field
toggle =
    boolField UiField.toggle


checkbox : WithLocale a -> Form FormError o -> String -> String -> Field
checkbox =
    boolField UiField.checkbox


{-| One of a set, stored as the chosen option's value in a string field.
-}
radioGroup : WithLocale a -> Form FormError o -> String -> String -> List (UiField.Choice Form.Msg) -> Field
radioGroup appState form path label options =
    let
        state =
            Form.getFieldAsString path form
    in
    UiField.radioGroup
        { id = fieldId path
        , label = label
        , value = Maybe.withDefault "" state.value
        , options = options
        , onChange = Form.Input path Form.Radio << Form.Field.String
        }
        |> finish appState path (errorText appState label state)


{-| Any number of a set, stored as **one boolean per option**, at
`<path>.<option value>`.

Not a list of strings: elm-form changes a list by appending an empty item and
then filling it in, which is two messages, and a checkbox sends one. A boolean
per option is one `Form.Input` per press, and the form's changed-fields tracking
then says exactly which options changed. Read the answer back in the validation:

    V.field "formats"
        (V.sequence
            (List.map
                (\value -> V.field value V.bool |> V.map (\on -> ( value, on )))
                formatValues
            )
            |> V.map (List.filter Tuple.second >> List.map Tuple.first)
        )

An error about the set as a whole — "choose at least one" — is an error on
`path` itself.

-}
checkboxGroup : WithLocale a -> Form FormError o -> String -> String -> List (UiField.Choice Form.Msg) -> Field
checkboxGroup appState form path label options =
    let
        optionPath value =
            path ++ "." ++ value

        isChecked value =
            Maybe.withDefault False (Form.getFieldAsBool (optionPath value) form).value

        checked =
            options
                |> List.map UiField.choiceValue
                |> List.filter isChecked
    in
    UiField.checkboxGroup
        { id = fieldId path
        , label = label
        , values = checked
        , options = options
        , onToggle = \value -> Form.Input (optionPath value) Form.Checkbox (Form.Field.Bool (not (isChecked value)))
        }
        |> finish appState path (groupErrorText appState label (Form.getFieldAsString path form))



-- EDITORS


{-| A calendar date, `2026-09-11`. The value in the form is that string; a form
that stores something else converts in its `init` and its validation.
-}
date : WithLocale a -> Form FormError o -> String -> String -> Field
date =
    textLike (withOnChange UiField.date) Form.Text


{-| An instant, ISO-8601 with a zone.
-}
dateTime : WithLocale a -> Form FormError o -> String -> String -> Field
dateTime =
    textLike (withOnChange UiField.dateTime) Form.Text


{-| A wall-clock time, `14:30`.
-}
time : WithLocale a -> Form FormError o -> String -> String -> Field
time =
    textLike (withOnChange UiField.time) Form.Text


{-| Markdown, in the design system's editor. This one field replaces the
wizard's three markdown form groups: the editor has its own formatted and source
modes, so there is no preview tab to build.
-}
markdown : WithLocale a -> Form FormError o -> String -> String -> Field
markdown =
    textLike (withOnChange UiField.markdown) Form.Textarea


{-| Code in `language` — one of the names `Ui.CodeBlock` lists.
-}
code : WithLocale a -> Form FormError o -> String -> String -> String -> Field
code appState form path label language =
    textLike
        (\config ->
            UiField.code
                { id = config.id
                , label = config.label
                , language = language
                , value = config.value
                , onChange = config.onInput
                }
        )
        Form.Textarea
        appState
        form
        path
        label


{-| A color as a hex string. `withSwatches` for a palette.
-}
color : WithLocale a -> Form FormError o -> String -> String -> Field
color =
    textLike UiField.color Form.Text


{-| A form value that is shown and not edited — an organization id that is
decided elsewhere. Text rather than a read-only input; see `Ui.Field.readOnly`.
For a value that is not in the form, use `Ui.Field.readOnly` directly.
-}
readOnly : Form FormError o -> String -> String -> Field
readOnly form path label =
    Field
        (UiField.readOnly
            { id = fieldId path
            , label = label
            , value = stringValue form path
            }
        )



-- VERSIONS


{-| A version as three whole numbers, one answer: `Ui.Field.group`, named by
`label` ("New version"), with fields labeled "Major", "Minor" and "Patch". The
group's name is said as focus enters it, so the short labels are enough, and
each field shows its own error from the form.

`hint` is under the numbers; `versionHint` explains what each one
means.

    Field.version appState
        form
        { majorField = "versionMajor"
        , minorField = "versionMinor"
        , patchField = "versionPatch"
        , label = gettext "Locale Version" appState.locale
        , hint = Nothing
        }

-}
version :
    WithLocale a
    -> Form FormError o
    ->
        { majorField : String
        , minorField : String
        , patchField : String
        , label : String
        , hint : Maybe String
        }
    -> Html Form.Msg
version appState form config =
    versionGroup appState form identity config
        |> UiField.groupToHtml


{-| A suggested version, and the words on its button — usually the version
itself, "2.0.0".
-}
type alias VersionSuggestion =
    { label : String
    , major : Int
    , minor : Int
    , patch : Int
    }


{-| `version`, with buttons under the numbers that fill in all three at once.

Choosing one changes three fields, which is three form messages and one press,
so it cannot be a `Form.Msg`: the button sends `onSuggestion`, and the page's
`update` applies it with `setVersion`. `versionSuggestions` gives the usual three
— the next major, minor and patch after the current version.

    Field.versionWithSuggestions appState
        model.form
        { majorField = "versionMajor"
        , minorField = "versionMinor"
        , patchField = "versionPatch"
        , label = gettext "New version" appState.locale
        , hint = Just (Field.versionHint appState)
        , suggestions = Field.versionSuggestions previousVersion
        , onSuggestion = FormSetVersion
        , toMsg = FormMsg
        }

-}
versionWithSuggestions :
    WithLocale a
    -> Form FormError o
    ->
        { majorField : String
        , minorField : String
        , patchField : String
        , label : String
        , hint : Maybe String
        , suggestions : List VersionSuggestion
        , onSuggestion : VersionSuggestion -> msg
        , toMsg : Form.Msg -> msg
        }
    -> Html msg
versionWithSuggestions appState form config =
    let
        suggestionButton suggestion =
            Button.ghost suggestion.label (config.onSuggestion suggestion)
                |> Button.small
                |> Button.toHtml

        actions =
            if List.isEmpty config.suggestions then
                []

            else
                Text.muted (gettext "Suggestions:" appState.locale)
                    :: List.map suggestionButton config.suggestions
    in
    versionGroup appState
        form
        config.toMsg
        { majorField = config.majorField
        , minorField = config.minorField
        , patchField = config.patchField
        , label = config.label
        , hint = config.hint
        }
        |> UiField.withGroupActions actions
        |> UiField.groupToHtml


{-| The explanation of the three numbers, for `hint`.
-}
versionHint : WithLocale a -> String
versionHint appState =
    gettext "The version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes, and number X indicates a major change." appState.locale


{-| The next major, minor and patch version after `current`; with no current
version, `1.0.0`, `0.1.0` and `0.0.1`.
-}
versionSuggestions : Maybe Version -> List VersionSuggestion
versionSuggestions current =
    let
        suggest next fallback =
            let
                suggested =
                    Maybe.unwrap fallback next current
            in
            { label = Version.toString suggested
            , major = Version.getMajor suggested
            , minor = Version.getMinor suggested
            , patch = Version.getPatch suggested
            }
    in
    [ suggest Version.nextMajor (Version.create 1 0 0)
    , suggest Version.nextMinor (Version.create 0 1 0)
    , suggest Version.nextPatch (Version.create 0 0 1)
    ]


{-| Puts a suggestion into the three fields, through the form's own `update`,
so the values are validated and marked as changed as if they had been typed.
-}
setVersion :
    Validation FormError o
    -> { a | majorField : String, minorField : String, patchField : String }
    -> { b | major : Int, minor : Int, patch : Int }
    -> Form FormError o
    -> Form FormError o
setVersion validation fields suggestion form =
    let
        set path value =
            Form.update validation (Form.Input path Form.Text (Form.Field.String (String.fromInt value)))
    in
    form
        |> set fields.majorField suggestion.major
        |> set fields.minorField suggestion.minor
        |> set fields.patchField suggestion.patch


versionGroup :
    WithLocale a
    -> Form FormError o
    -> (Form.Msg -> msg)
    ->
        { majorField : String
        , minorField : String
        , patchField : String
        , label : String
        , hint : Maybe String
        }
    -> UiField.FieldGroup msg
versionGroup appState form toMsg config =
    let
        part path label =
            number appState form path label
                |> withRequired True
                |> withPlaceholder "0"
                |> toHtml
                |> Html.map toMsg
    in
    UiField.group
        { id = fieldId config.majorField ++ "-version"
        , legend = config.label
        , fields =
            [ part config.majorField (gettext "Major" appState.locale)
            , part config.minorField (gettext "Minor" appState.locale)
            , part config.patchField (gettext "Patch" appState.locale)
            ]
        }
        |> (case config.hint of
                Just hint ->
                    UiField.withGroupHint hint

                Nothing ->
                    identity
           )



-- FILES


{-| A `Ui.FileUpload` in a form, with the form's timing for its error: a required
upload with nothing chosen says "File is required." once the form has been
submitted, like any other empty required field, and not before.

The file is not in the form — elm-form holds strings and booleans — so the page
still checks for it before sending; this only says so. `path` is used for the
ids (`form-<path>`), as a field's would be, so it needs to be unique in the form
and need not be a real form field.

-}
fileUpload :
    WithLocale a
    -> Form FormError o
    ->
        { path : String
        , label : String
        , buttonLabel : String
        , hint : String
        , required : Bool
        , upload : FileUpload.Model
        , toMsg : FileUpload.Msg -> msg
        , onClear : Maybe msg
        }
    -> Html msg
fileUpload appState form config =
    fileUploadWithTestId appState
        form
        { path = config.path
        , label = config.label
        , buttonLabel = config.buttonLabel
        , hint = config.hint
        , required = config.required
        , upload = config.upload
        , toMsg = config.toMsg
        , onClear = config.onClear
        , testId = Nothing
        }


{-| `fileUpload`, with a `data-cy` on the field for the end-to-end tests.
-}
fileUploadWithTestId :
    WithLocale a
    -> Form FormError o
    ->
        { path : String
        , label : String
        , buttonLabel : String
        , hint : String
        , required : Bool
        , upload : FileUpload.Model
        , toMsg : FileUpload.Msg -> msg
        , onClear : Maybe msg
        , testId : Maybe String
        }
    -> Html msg
fileUploadWithTestId appState form config =
    FileUpload.toHtmlWithErrorAndTestId (UiWords.fileUpload appState)
        { id = fieldId config.path
        , toMsg = config.toMsg
        , label = config.label
        , buttonLabel = config.buttonLabel
        , hint = config.hint
        , error =
            case FileUpload.file config.upload of
                Nothing ->
                    if config.required && Form.isSubmitted form then
                        Just (gettext "File is required." appState.locale)

                    else
                        Nothing

                Just _ ->
                    Nothing
        , onClear = config.onClear
        , testId = config.testId
        }
        config.upload



-- LISTS


{-| An elm-form list, as a `Ui.ListField`: a group named `label`, one row per
item, a remove button on each row and one add button after them.

`row` is given the item's index and returns the fields for that item — build
their paths with `listItemPath`. `rowLabel` names the row among the others,
which is what its remove button is called after; a value from the row is best
(the parameter's name). A row with no name is called by its position.

An error about the list as a whole — "at least one is required" — is shown under
it; errors about a row's fields are on those fields.

-}
list :
    WithLocale a
    -> Form FormError o
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html Form.Msg)
        }
    -> Html Form.Msg
list appState form config =
    listMapped appState
        form
        identity
        { field = config.field
        , label = config.label
        , addLabel = config.addLabel
        , rowLabel = config.rowLabel
        , row = config.row
        , rowActions = always []
        }


{-| `list`, with a sentence about the list as a whole under it. The hint is in
the group's `aria-describedby`, so it is read as the list is entered — which a
paragraph placed after the list is not.
-}
listWithHint :
    String
    -> WithLocale a
    -> Form FormError o
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html Form.Msg)
        }
    -> Html Form.Msg
listWithHint hint appState form config =
    viewListWithHint (Just [ text hint ]) appState form config


{-| `listWithHint`, with the sentence written in markdown — a name in code, an
emphasis, a link. Plain text belongs in `listWithHint`, which parses nothing.
-}
listWithHintMarkdown :
    String
    -> WithLocale a
    -> Form FormError o
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html Form.Msg)
        }
    -> Html Form.Msg
listWithHintMarkdown hint appState form config =
    viewListWithHint (Just (Markdown.inline hint)) appState form config


{-| `list`, for rows whose content sends something other than `Form.Msg` — a row
with "Move up" and "Move down" buttons that are the page's messages. The rows
are in the page's message; the fields inside them are mapped with `toMsg` by the
caller, and adding and removing are mapped here.

    Field.listMapped appState form SettingsPage.formMsg
        { field = "customMenuLinks"
        , label = gettext "Custom Menu Links" appState.locale
        , addLabel = gettext "Add link" appState.locale
        , rowLabel = \index -> …
        , row = \index -> [ Html.map SettingsPage.formMsg …, moveButtons index ]
        }

-}
listMapped :
    WithLocale a
    -> Form FormError o
    -> (Form.Msg -> msg)
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html msg)
        , rowActions : Int -> List (Button msg)
        }
    -> Html msg
listMapped =
    viewList Nothing


{-| The list as `listWithHint` and `listWithHintMarkdown` have it: rows that
send `Form.Msg` and no actions of their own.
-}
viewListWithHint :
    Maybe (List (Html Form.Msg))
    -> WithLocale a
    -> Form FormError o
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html Form.Msg)
        }
    -> Html Form.Msg
viewListWithHint hint appState form config =
    viewList hint
        appState
        form
        identity
        { field = config.field
        , label = config.label
        , addLabel = config.addLabel
        , rowLabel = config.rowLabel
        , row = config.row
        , rowActions = always []
        }


viewList :
    Maybe (List (Html msg))
    -> WithLocale a
    -> Form FormError o
    -> (Form.Msg -> msg)
    ->
        { field : String
        , label : String
        , addLabel : String
        , rowLabel : Int -> String
        , row : Int -> List (Html msg)
        , rowActions : Int -> List (Button msg)
        }
    -> Html msg
viewList hint appState form toMsg config =
    let
        toRow index =
            ListField.row
                { label = rowName appState (config.rowLabel index) index
                , content = config.row index
                }
                |> ListField.withRowActions (config.rowActions index)
                -- The item's form path, as its fields' ids are: `parameters.0_row`.
                |> ListField.withRowTestId (fieldId (listItemPath config.field index "") ++ "_row")

        state =
            Form.getFieldAsString config.field form
    in
    ListField.toHtmlWithWords (UiWords.listField appState)
        { id = fieldId config.field
        , label = config.label
        , rows = List.map toRow (Form.getListIndexes config.field form)
        , addLabel = config.addLabel
        , onAdd = toMsg (Form.Append config.field)
        , onRemove = Just (toMsg << Form.RemoveItem config.field)
        , hint = hint
        , error = groupErrorText appState config.label state
        }


{-| The path of a field inside a list item: `listItemPath "parameters" 0 "name"`
is `parameters.0.name`. An item that is a plain value rather than a group has no
field of its own — pass `""` and get `projectTags.0`.
-}
listItemPath : String -> Int -> String -> String
listItemPath listPath index field =
    let
        itemPath =
            listPath ++ "." ++ String.fromInt index
    in
    if String.isEmpty field then
        itemPath

    else
        itemPath ++ "." ++ field


rowName : WithLocale a -> String -> Int -> String
rowName appState name index =
    if String.isEmpty (String.trim name) then
        String.format (gettext "Item %s" appState.locale) [ String.fromInt (index + 1) ]

    else
        name



-- BUILDERS


{-| Put `prefix` in front of the id `fieldId` gives the field, for a form that is
on the page more than once — a create and an edit dialog with the same fields.
`withIdPrefix "edit-secret-"` makes `name` `edit-secret-form-name`. Anything
that finds the field by `fieldId` has to add the same prefix; the first-error
focus of `Common.Components.Form` does not look at ids, so it is unaffected.
-}
withIdPrefix : String -> Field -> Field
withIdPrefix prefix =
    mapUi (UiField.withIdPrefix prefix)


withHint : String -> Field -> Field
withHint hint =
    mapUi (UiField.withHint hint)


{-| A hint written in markdown — a name in code, an emphasis, a link — as the
existing settings write them. Plain text belongs in `withHint`, which parses
nothing: most hints are a sentence and have no markdown in them.
-}
withHintMarkdown : String -> Field -> Field
withHintMarkdown hint =
    mapUi (UiField.withHintHtml (Markdown.inline hint))


{-| A placeholder is an example, never the label: it disappears as soon as
anything is typed.
-}
withPlaceholder : String -> Field -> Field
withPlaceholder placeholder =
    mapUi (UiField.withPlaceholder placeholder)


{-| Marks the field as required. It does not make it required — the validation
does — and the form is `novalidate`, so the browser does not add a check of its
own.
-}
withRequired : Bool -> Field -> Field
withRequired required =
    mapUi (UiField.withRequired required)


{-| What changing the value will do, or what a valid value will not do — see
`Ui.Field.withWarning`.
-}
withWarning : Maybe String -> Field -> Field
withWarning warning =
    mapUi (UiField.withWarning warning)


{-| How many lines a text box shows — see `Ui.Field.withRows`.
-}
withRows : Int -> Field -> Field
withRows count =
    mapUi (UiField.withRows count)


withDisabled : Bool -> Field -> Field
withDisabled disabled =
    mapUi (UiField.withDisabled disabled)


{-| A select that cannot be changed, with the reason as its tooltip — see
`Ui.Field.withDisabledReason`.
-}
withDisabledReason : { id : String, reason : String } -> Field -> Field
withDisabledReason reason =
    mapUi (UiField.withDisabledReason reason)


withAutocomplete : String -> Field -> Field
withAutocomplete token =
    mapUi (UiField.withAutocomplete token)


withPrefix : String -> Field -> Field
withPrefix prefix =
    mapUi (UiField.withPrefix prefix)


withSuffix : String -> Field -> Field
withSuffix suffix =
    mapUi (UiField.withSuffix suffix)


{-| The icon the field's value names, drawn after it (`Ui.Field.withSuffixIcon`).
-}
withSuffixIcon : Icon -> Field -> Field
withSuffixIcon icon =
    mapUi (UiField.withSuffixIcon icon)


{-| For fields inside a list row, where the column already says what they are.
See `Ui.Field.withHiddenLabel` for how narrow an exception this is.
-}
withHiddenLabel : Field -> Field
withHiddenLabel =
    mapUi UiField.withHiddenLabel


{-| Values to offer while typing, which do not restrict what may be typed. See
`Ui.Field.withSuggestions`.
-}
withSuggestions : List String -> Field -> Field
withSuggestions suggestions =
    mapUi (UiField.withSuggestions suggestions)


withSwatches : List String -> Field -> Field
withSwatches swatches =
    mapUi (UiField.withSwatches swatches)


{-| A button that empties a color field with a default. See
`Ui.Field.withClear`.
-}
withClear : Field -> Field
withClear =
    mapUi UiField.withClear


toHtml : Field -> Html Form.Msg
toHtml (Field field) =
    UiField.toHtml field



-- ERRORS AND IDS


{-| The message for a field's live error, if it has one, worded by
`Common.Utils.Form`.
-}
errorText : WithLocale a -> String -> Form.FieldState FormError String -> Maybe String
errorText appState label state =
    FormUtils.fieldErrorToString appState.locale label state


{-| The message for an error on something that has no string value of its own —
a list, a group of checkboxes. `errorText` decides "is empty" from the value, and
these never have one, so it would call every error on them empty.
-}
groupErrorText : WithLocale a -> String -> Form.FieldState FormError String -> Maybe String
groupErrorText appState label state =
    Maybe.map (FormUtils.errorToString appState.locale label) state.liveError


{-| The DOM id of the field at `path`: `form-` and the path, with anything that
is not a letter, a digit, `-` or `_` turned into a hyphen and runs of them
collapsed. `parameters.0.name` is `form-parameters-0-name`.
-}
fieldId : String -> String
fieldId path =
    let
        keep char =
            Char.isAlphaNum char || char == '-' || char == '_'
    in
    path
        |> String.map
            (\char ->
                if keep char then
                    char

                else
                    '-'
            )
        |> String.split "-"
        |> List.filterNot String.isEmpty
        |> String.join "-"
        |> (++) "form-"



-- INTERNAL


type alias TextConfig =
    { id : String, label : String, value : String, onInput : String -> Form.Msg }


textLike :
    (TextConfig -> UiField.Field Form.Msg)
    -> Form.InputType
    -> WithLocale a
    -> Form FormError o
    -> String
    -> String
    -> Field
textLike toField inputType appState form path label =
    let
        state =
            Form.getFieldAsString path form
    in
    toField
        { id = fieldId path
        , label = label
        , value = Maybe.withDefault "" state.value
        , onInput = Form.Input path inputType << Form.Field.String
        }
        |> finish appState path (errorText appState label state)


withOnChange :
    ({ id : String, label : String, value : String, onChange : String -> Form.Msg } -> UiField.Field Form.Msg)
    -> TextConfig
    -> UiField.Field Form.Msg
withOnChange toField config =
    toField { id = config.id, label = config.label, value = config.value, onChange = config.onInput }


boolField :
    ({ id : String, label : String, checked : Bool, onCheck : Bool -> Form.Msg } -> UiField.Field Form.Msg)
    -> WithLocale a
    -> Form FormError o
    -> String
    -> String
    -> Field
boolField toField appState form path label =
    let
        state =
            Form.getFieldAsBool path form
    in
    toField
        { id = fieldId path
        , label = label
        , checked = Maybe.withDefault False state.value
        , onCheck = Form.Input path Form.Checkbox << Form.Field.Bool
        }
        |> finish appState path (Maybe.map (FormUtils.errorToString appState.locale label) state.liveError)


{-| What every field gets: its own words in the page's language, the error, and
focus and blur reported to the form so that `liveError` is released at the right
moment.
-}
finish : WithLocale a -> String -> Maybe String -> UiField.Field Form.Msg -> Field
finish appState path error field =
    field
        |> UiField.withWords (UiWords.field appState)
        |> UiField.withError error
        |> UiField.withOnFocus (Form.Focus path)
        |> UiField.withOnBlur (Form.Blur path)
        |> Field


{-| A radio or checkbox group's options in a row, wrapping only when narrow
(`Ui.Field.withChoicesInline`).
-}
withChoicesInline : Field -> Field
withChoicesInline =
    mapUi UiField.withChoicesInline


mapUi : (UiField.Field Form.Msg -> UiField.Field Form.Msg) -> Field -> Field
mapUi fn (Field field) =
    Field (fn field)


stringValue : Form FormError o -> String -> String
stringValue form path =
    Maybe.withDefault "" (Form.getFieldAsString path form).value


intValue : Form FormError o -> String -> Int
intValue form path =
    Maybe.withDefault 0 (String.toInt (stringValue form path))
