module Ui.Modal exposing
    ( Modal
    , confirm
    , plain
    , toHtml
    , withActions
    , withCancelLabel
    , withContentOnly
    , withCover
    , withDangerConfirm
    , withFocusOnTitle
    , withForm
    , withHeaderAction
    , withHeaderControl
    , withId
    , withLoading
    , withStartAction
    , withTestId
    , withWide
    )

{-| Built on the native `<dialog>` through the `ui-dialog` custom element.

Focus trapping, Escape, the top layer and an inert background all come from the
browser. A hand-rolled focus trap is one of the easiest things in this system to
get subtly wrong, and none of it is the caller's responsibility: no caller can
forget the behavior because no caller is involved in it.

Focus in and out is the browser's too. `showModal` moves focus to the first
focusable thing inside the dialog, and closing it returns focus to whatever had
it before — the trigger, in every case that matters. The header holds no
control, so in a form modal the first focusable thing is the first field, which
is exactly where focus should start; that is why there is no `autofocus`
builder. If a form's body opens with a link or a button of its own, focus lands
there instead — put the fields first.

The exception is `withFocusOnTitle`, for a form whose first field is not where
most people start: one that opens a list of suggestions on focus, over the
settings the dialog was opened to look at.

-}

import Html exposing (Html, div, form, h2, node, text)
import Html.Attributes exposing (attribute, class, id, novalidate)
import Html.Events exposing (on, onSubmit)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Button as Button exposing (Button)
import Ui.TestId as TestId


type Modal msg
    = Modal (Config msg)


type alias Config msg =
    { title : String
    , body : List (Html msg)
    , open : Bool
    , onClose : msg
    , action : Maybe (Action msg)
    , loading : Bool
    , dangerous : Bool
    , wide : Bool
    , cover : Bool
    , form : Maybe (FormConfig msg)
    , actions : Maybe (List (Button msg))
    , startAction : Maybe (Button msg)
    , testId : Maybe String
    , id : Maybe String
    , cancelLabel : Maybe String
    , focusOnTitle : Bool
    , headerAction : Maybe (Html msg)
    , contentOnly : Bool
    }


type alias Action msg =
    { label : String
    , onConfirm : msg
    }


type alias FormConfig msg =
    { onSubmit : msg
    , submitLabel : String
    }


plain : { title : String, open : Bool, onClose : msg, body : List (Html msg) } -> Modal msg
plain config =
    Modal
        { title = config.title
        , body = config.body
        , open = config.open
        , onClose = config.onClose
        , action = Nothing
        , loading = False
        , dangerous = False
        , wide = False
        , cover = False
        , form = Nothing
        , actions = Nothing
        , startAction = Nothing
        , testId = Nothing
        , id = Nothing
        , cancelLabel = Nothing
        , focusOnTitle = False
        , headerAction = Nothing
        , contentOnly = False
        }


confirm :
    { title : String
    , open : Bool
    , onClose : msg
    , body : List (Html msg)
    , confirmLabel : String
    , onConfirm : msg
    }
    -> Modal msg
confirm config =
    let
        (Modal base) =
            plain
                { title = config.title
                , open = config.open
                , onClose = config.onClose
                , body = config.body
                }
    in
    Modal { base | action = Just { label = config.confirmLabel, onConfirm = config.onConfirm } }


withDangerConfirm : Bool -> Modal msg -> Modal msg
withDangerConfirm dangerous (Modal config) =
    Modal { config | dangerous = dangerous }


{-| Puts the confirm or submit button into its loading state. It stays
disabled while loading, and a disabled submit button also blocks Enter and
Cmd/Ctrl+Enter, so a form cannot be sent twice from the keyboard either.
-}
withLoading : Bool -> Modal msg -> Modal msg
withLoading loading (Modal config) =
    Modal { config | loading = loading }


{-| Focus the dialog's title when it opens, rather than its first control.

For a dialog that is opened to be read before anything in it is changed, and
whose first control does something on focus — a people picker that drops its
suggestions over the rest of the dialog, say. The title is announced, which
says where focus is; Tab from there reaches the first control, and closing
still returns focus to the trigger.

The title is focusable from script only (`tabindex="-1"`), so it is never a Tab
stop, and it carries `autofocus`: when a dialog opens, the browser focuses the
first element inside it with `autofocus` before it falls back to the first
control — the platform's own rule, in every browser, and no script races it.
Focusing the `dialog` itself does not do it: the browser hands that focus on to
the first control.

Not for a dialog opened to type into: there, the first field is where focus
should start, and the default puts it there.

-}
withFocusOnTitle : Modal msg -> Modal msg
withFocusOnTitle (Modal config) =
    Modal { config | focusOnTitle = True }


{-| One control at the end of the title's line — the guide link
(`Common.Components.GuideLink.button`), where page headers put theirs.

It is the dialog's first control, so the browser would give it the focus on
opening; a form that should still start at its first field says so with
`withFocusOnTitle`, and Tab from the title reaches the action and then the
fields, in the order they are read.

-}
withHeaderAction : Button msg -> Modal msg -> Modal msg
withHeaderAction action =
    withHeaderControl (Button.toHtml action)


{-| `withHeaderAction` for a control that is not a `Button` — the about
dialog's `Ui.Copy.labeledWithWords`.
-}
withHeaderControl : Html msg -> Modal msg -> Modal msg
withHeaderControl control (Modal config) =
    Modal { config | headerAction = Just control }


{-| A larger maximum width, for content that has to be read side by side — a
table, a changelog, a plugin's own settings. It is still capped by the viewport,
so at 320px a wide dialog is exactly as wide as a narrow one (1.4.10).
-}
withWide : Modal msg -> Modal msg
withWide (Modal config) =
    Modal { config | wide = True }


{-| As much of the window as a dialog can take, for content that is a page in
its own right — a questionnaire as it was at some point in its history.

The body keeps its own scrolling, so what is shown behaves as it does in the
page; what changes is only how much room the dialog asks for.

-}
withCover : Modal msg -> Modal msg
withCover (Modal config) =
    Modal { config | cover = True }


{-| The body and the footer become a `form`.

The primary action is a `type="submit"` button with no click handler of its
own, so Cmd/Ctrl+Enter and a click on the button send the same `onSubmit` from
the same place. Enter does not submit: in a dialog with a form or a field in it,
Enter is the fields' own — a picker takes a suggestion with it, a select an
option — and the shortcut saves from anywhere in the dialog (`ui-dialog.js`). `novalidate` keeps the browser's own bubbles
out of it — validation is the caller's, and it is shown the way every other
form shows it (`Ui.Form`). Cancel stays `type="button"`: it must never submit.

With `withDangerConfirm` the submit button takes the danger color, as a
confirm button does: a form that destroys something is still a dialog asking
whether to destroy it. `withActions` replaces the footer here too, and then the
caller supplies the `Button.submit` or `Button.dangerSubmit` itself.

-}
withForm : { onSubmit : msg, submitLabel : String } -> Modal msg -> Modal msg
withForm formConfig (Modal config) =
    Modal { config | form = Just formConfig }


{-| A footer of the caller's buttons, in place of the default dismiss, or
cancel and confirm. The buttons are rendered in the order given, and that order
is the tab order; put the primary action last, as the default footer does.

Nothing is added — include a dismissing button yourself, because Escape and the
backdrop are not discoverable to everyone.

An empty list is a dialog with no footer at all, for one whose content is a
page in its own right and whose way out is an action in its header
(`withHeaderAction`). The element is kept either way, and only the room it
takes is given up.

-}
withActions : List (Button msg) -> Modal msg -> Modal msg
withActions actions (Modal config) =
    Modal { config | actions = Just actions }


{-| A button at the start of the footer, apart from the others — "Discard" across
from "Cancel" and "Save": a way out that is not the way back, kept away from the
button that is pressed most. It comes first in the tab order, as it is read.
-}
withStartAction : Button msg -> Modal msg -> Modal msg
withStartAction action (Modal config) =
    Modal { config | startAction = Just action }


{-| The id the dialog's title gets, and so the name of the dialog: the dialog is
`aria-labelledby` its title.

Without it the id is `ui-dialog-` and a slug of the title, which is unique as
long as no two dialogs on the page share a title. Two that do — a create and an
edit dialog both called "Secret", say — each need a `withId`.

-}
withId : String -> Modal msg -> Modal msg
withId value (Modal config) =
    Modal { config | id = Just value }


{-| Always there when asked for, not only while the dialog is open: the element
opens the dialog when its `open` attribute changes, which the virtual DOM sets
before it patches anything inside — `autofocus` added in the same render would
arrive after the browser had already chosen. A closed dialog is not rendered, so
nothing in it can take focus in the meantime.
-}
titleFocus : Config msg -> List (Html.Attribute msg)
titleFocus config =
    if config.focusOnTitle then
        [ attribute "tabindex" "-1", attribute "autofocus" "" ]

    else
        []


titleId : Config msg -> String
titleId config =
    case config.id of
        Just value ->
            value ++ "-title"

        Nothing ->
            "ui-dialog-" ++ slug config.title ++ "-title"


slug : String -> String
slug value =
    String.toLower value
        |> String.map
            (\char ->
                if Char.isAlphaNum char then
                    char

                else
                    '-'
            )
        |> String.split "-"
        |> List.filterNot String.isEmpty
        |> String.join "-"


{-| A `data-cy` on the dialog's element. The buttons the dialog draws itself
take one derived from it: `<testId>_confirm` and `<testId>_cancel` for a
`confirm`, `<testId>_close` for a plain dialog. A form's submit and cancel stay
`form-submit` and `form-cancel`, inside the dialog's element.
-}
withTestId : String -> Modal msg -> Modal msg
withTestId testId (Modal config) =
    Modal { config | testId = Just testId }


{-| A dialog whose content is all the caller's: no title drawn and no footer.

For content that is a page in its own right and brings its own heading and its
own controls — a plugin's element, which renders the whole of what it is for and
closes itself. The title is still given, and becomes the dialog's `aria-label`:
a dialog with no name at all is announced as "dialog" and nothing more.

Everything the dialog would have offered goes with the chrome, the way out
included, so the content must carry one (a plugin's `onActionClose`). Escape and
the backdrop still work, but neither is discoverable to everyone.

-}
withContentOnly : Modal msg -> Modal msg
withContentOnly (Modal config) =
    Modal { config | contentOnly = True }


{-| The words on the button that dismisses the dialog: "Cancel" beside a
confirm or a submit, "Close" on a plain dialog. Both defaults are English, and
the label is the caller's to translate.
-}
withCancelLabel : String -> Modal msg -> Modal msg
withCancelLabel label (Modal config) =
    Modal { config | cancelLabel = Just label }


toHtml : Modal msg -> Html msg
toHtml (Modal config) =
    let
        openAttrs =
            if config.open then
                [ attribute "open" "" ]

            else
                []

        -- `open` is the element's, which opens the dialog as a modal: on the
        -- dialog itself it would show it without the backdrop or the inert page.
        hostAttrs =
            Maybe.values
                [ if config.cover then
                    Just (attribute "data-size" "cover")

                  else if config.wide then
                    Just (attribute "data-size" "wide")

                  else
                    Nothing
                , Maybe.map TestId.attribute config.testId
                ]

        -- The dialog is named by its title, or, where no title is drawn to point
        -- at, by the title itself.
        name =
            if config.contentOnly then
                attribute "aria-label" config.title

            else
                attribute "aria-labelledby" (titleId config)
    in
    -- The dialog is drawn here, not made by the element: the virtual DOM then
    -- knows where everything in it is. An element it keeps from one page to the
    -- next — the same tag in the same place — may become another dialog, and
    -- what that one has more of goes into the dialog, not beside it on the page.
    node "ui-dialog"
        (on "dialogClose" (Decode.succeed config.onClose) :: openAttrs ++ hostAttrs)
        [ node "dialog"
            [ class "ui-dialog", name ]
            (if config.contentOnly then
                [ div [ class "ui-dialog__content-only" ] config.body ]

             else
                chrome config
            )
        ]


{-| The dialog's own parts: a header, the body and a footer.

Its own function so that a dialog whose content is all the caller's
(`withContentOnly`) builds none of them.

-}
chrome : Config msg -> List (Html msg)
chrome config =
    let
        cancelLabel default =
            Maybe.withDefault default config.cancelLabel

        -- A dialog with a test id gives its own buttons one derived from it,
        -- so a test presses "the confirm of this dialog" without its words.
        withDerivedTestId suffix button =
            case config.testId of
                Just testId ->
                    Button.withTestId (testId ++ "_" ++ suffix) button

                Nothing ->
                    button

        confirmButton action =
            (if config.dangerous then
                Button.danger action.label action.onConfirm

             else
                Button.primary action.label action.onConfirm
            )
                |> withDerivedTestId "confirm"

        submitButton label =
            if config.dangerous then
                Button.dangerSubmit label

            else
                Button.submit label

        footerButtons =
            case ( config.actions, config.form, config.action ) of
                ( Just actions, _, _ ) ->
                    actions

                ( Nothing, Just formConfig, _ ) ->
                    [ Button.ghost (cancelLabel "Cancel") config.onClose
                        |> Button.withTestId "form-cancel"
                    , Button.withLoading config.loading (submitButton formConfig.submitLabel)
                    ]

                ( Nothing, Nothing, Just action ) ->
                    [ Button.ghost (cancelLabel "Cancel") config.onClose
                        |> withDerivedTestId "cancel"
                    , Button.withLoading config.loading (confirmButton action)
                    ]

                -- Alone, the way out is the dialog's action: primary, so
                -- that it takes the first focus and Enter presses it.
                ( Nothing, Nothing, Nothing ) ->
                    [ Button.primary (cancelLabel "Close") config.onClose
                        |> withDerivedTestId "close"
                    ]

        body =
            div [ class "ui-dialog__body" ] config.body

        footer =
            div [ class "ui-dialog__footer" ]
                ((case config.startAction of
                    Just action ->
                        [ div [ class "ui-dialog__footer-start" ] [ Button.toHtml action ] ]

                    Nothing ->
                        []
                 )
                    ++ List.map Button.toHtml footerButtons
                )

        content =
            case config.form of
                Just formConfig ->
                    [ form
                        [ class "ui-dialog__form"
                        , novalidate True
                        , onSubmit formConfig.onSubmit
                        ]
                        [ body, footer ]
                    ]

                Nothing ->
                    [ body, footer ]
    in
    -- An h2: a dialog is a context of its own over the page, and an h2 under
    -- the page's h1 never skips a level, whatever the page has. Headings in
    -- the dialog's body start at h3.
    div [ class "ui-dialog__header" ]
        (h2 (class "ui-subtitle" :: id (titleId config) :: titleFocus config) [ text config.title ]
            :: (case config.headerAction of
                    Just control ->
                        [ control ]

                    Nothing ->
                        []
               )
        )
        :: content
