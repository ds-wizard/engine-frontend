module Common.Components.Form exposing
    ( actions
    , cancelFocusFirstField
    , createPage
    , createView
    , createWithinPage
    , errorSummary
    , errorSummaryId
    , focusErrorSummary
    , focusFirstField
    , reloadWhenSaved
    , submitShortcut
    , unsavedBar
    , view
    )

{-| Everything around the fields of an `enbala/elm-form` form: the `form`
element, the summary of a server error, the actions, the unsaved-changes bar,
the shortcut, and where focus goes when something is wrong. The fields
themselves are `Common.Components.Form.Field`.

The rules this module exists to keep, of `Ui` and of this tier:

  - **Save is never disabled on validity.** A disabled Save says nothing about
    what is wrong and leaves a keyboard on a dead control. Nothing here takes a
    validity flag, so there is nowhere to pass one.
  - **An invalid submit moves focus to the first invalid field**, and scrolls
    it into view. Its inline error is already showing — elm-form releases every
    error on submit — so the field is the explanation, and there is no second,
    client-side summary repeating it. Nothing to call: it follows the form's
    submit, the unsaved bar's Save and the shortcut on their own
    (`packages/elm-ui/js/form-errors.js`).
  - **A server error goes in a summary, and focus goes to it.** It has no field
    to sit beside. Call `focusErrorSummary` when the request fails.
  - **Cmd/Ctrl+Enter submits.** A subscription, `submitShortcut`, not an
    element around the form.
  - **A create form opens on its first field.** It is rendered with
    `createView` (which `createPage` does), and the application sends
    `focusFirstField` instead of focusing the page's heading when it navigates
    to one — there is one thing to do on the page, and it starts with typing.

A create page is these pieces in order:

    Form.createPage appState
        { title = gettext "Create User" appState.locale
        , description = Nothing
        , actions = []
        , content = List.map (Html.map FormMsg) fields
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingUser
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }

-}

import ActionResult exposing (ActionResult(..))
import Common.Data.Context exposing (WithLocale, WithNavigator)
import Common.Ports.Dom as Dom
import Common.Ports.Form as Ports
import Common.Ports.Window as Window
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (id, novalidate, tabindex)
import Html.Events exposing (onSubmit)
import Ui.Alert as Alert
import Ui.Button as Button exposing (Button)
import Ui.Form
import Ui.Layout as Layout
import Ui.Page.Content as Content


{-| The `form` element: `novalidate`, because the errors are elm-form's and are
said in words beside each field, and `onSubmit`, because that is the one place
the submit is sent from — Enter in a text field, the submit button and the
shortcut all end up here or at the same message.

Anything using `actions` must be inside one, since the submit button is a real
`type="submit"` with no handler of its own.

-}
view : { onSubmit : msg, content : List (Html msg) } -> Html msg
view config =
    Html.form [ novalidate True, onSubmit config.onSubmit ]
        [ Layout.stack config.content ]


{-| `view`, for the form of a page that creates something. It carries the id
`focusFirstField` looks in, so the page opens with its first field focused.
-}
createView : { onSubmit : msg, content : List (Html msg) } -> Html msg
createView config =
    Html.form [ id createFormId, novalidate True, onSubmit config.onSubmit ]
        [ Layout.stack config.content ]


createFormId : String
createFormId =
    "c-create-form"


{-| Focus the first field a person types into — not a switch or a choice — of
the create form on the page, once it is rendered. For the application to send
when it opens a create page, in place of focusing the heading. It waits for a
form that is still loading and gives up after a few seconds.
-}
focusFirstField : Cmd msg
focusFirstField =
    Ports.focusFirstFieldIn createFormId


{-| Drop a `focusFirstField` still waiting for its form. For every other
navigation, so a create page left before it loaded does not take focus back.
-}
cancelFocusFirstField : Cmd msg
cancelFocusFirstField =
    Ports.cancelFocusFirstField


{-| The id of the server error summary, for anything that moves focus to it
other than `focusErrorSummary`.
-}
errorSummaryId : String
errorSummaryId =
    "c-api-error"


{-| The server's error, when the result is one. Nothing otherwise.

The message is already translated — `ApiError.toActionResult` has done that — so
this only places it. The alert announces itself; the wrapper takes focus
(`tabindex -1`) so that `focusErrorSummary` can put the reader where the problem
is said, rather than leaving them on a submit button that appears to have done
nothing.

`Common.Components.ApiErrorAlert.view` renders through this, with the same id,
so there is one server-error element in the system and one id to focus.

-}
errorSummary : ActionResult x -> Html msg
errorSummary result =
    case result of
        Error message ->
            div [ id errorSummaryId, tabindex -1 ]
                [ Alert.toHtml (Alert.danger message) ]

        _ ->
            Html.text ""


{-| The submit, and Cancel when there is somewhere to go back to.

With Cancel it is a create form's footer (`Ui.Form.footer`): a line under the
fields, Cancel at the start and the submit at the end. Without one — signing in,
saving a profile — the submit stands alone under the fields.

The submit is busy while the request is out — `aria-busy`, and a busy submit
button also stops a second submit by Enter — and it is never disabled for any
other reason.

-}
actions :
    WithLocale a
    ->
        { submitLabel : String
        , result : ActionResult x
        , onCancel : Maybe msg
        }
    -> Html msg
actions appState config =
    let
        submit =
            Button.withLoading (ActionResult.isLoading config.result) (Button.submit config.submitLabel)
    in
    case config.onCancel of
        Just msg ->
            Ui.Form.footer
                { cancel =
                    Button.ghost (gettext "Cancel" appState.locale) msg
                        |> Button.withTestId "form-cancel"
                , submit = submit
                }

        Nothing ->
            Ui.Form.actions [ submit ]


{-| A page whose whole purpose is one form: a narrow `Ui.Page.Content` with the
title, the server error, the fields and the actions, inside `view`.

`actions` are the header's — a guide link, usually — not the form's.

-}
createPage :
    WithLocale a
    ->
        { title : String
        , description : Maybe String
        , actions : List (Button msg)
        , content : List (Html msg)
        , submitLabel : String
        , result : ActionResult x
        , onSubmit : msg
        , onCancel : Maybe msg
        }
    -> Html msg
createPage appState config =
    createContent appState config
        |> Content.toHtml


{-| `createPage`, for a create form that is a region of a page which already has
a heading — the new document form on a project's documents tab.

`Ui.Page.Content.withinPage`: the title is the section's `h2` rather than the
page's `h1`, and the scrolling stays the containing template's.

-}
createWithinPage :
    WithLocale a
    ->
        { title : String
        , description : Maybe String
        , actions : List (Button msg)
        , content : List (Html msg)
        , submitLabel : String
        , result : ActionResult x
        , onSubmit : msg
        , onCancel : Maybe msg
        }
    -> Html msg
createWithinPage appState config =
    createContent appState config
        |> Content.withinPage
        |> Content.toHtml


createContent :
    WithLocale a
    ->
        { c
            | title : String
            , description : Maybe String
            , actions : List (Button msg)
            , content : List (Html msg)
            , submitLabel : String
            , result : ActionResult x
            , onSubmit : msg
            , onCancel : Maybe msg
        }
    -> Content.Content msg
createContent appState config =
    let
        withDescription page =
            case config.description of
                Just description ->
                    Content.withDescription description page

                Nothing ->
                    page
    in
    Content.content
        { title = config.title
        , body =
            [ createView
                { onSubmit = config.onSubmit
                , content =
                    errorSummary config.result
                        :: config.content
                        ++ [ actions appState
                                { submitLabel = config.submitLabel
                                , result = config.result
                                , onCancel = config.onCancel
                                }
                           ]
                }
            ]
        }
        |> Content.narrow
        |> Content.withActions config.actions
        |> withDescription


{-| Cmd+Enter on a Mac, Ctrl+Enter elsewhere. Nothing while the request is out,
so holding the keys does not send the form twice.

Subscribe only while the form is on screen.

-}
submitShortcut : WithNavigator a -> { result : ActionResult x, onSubmit : msg } -> Sub msg
submitShortcut appState config =
    if ActionResult.isLoading config.result then
        Sub.none

    else
        Ui.Form.submitShortcut { isMac = appState.navigator.isMac, onSubmit = config.onSubmit }


{-| The bar for editing something that exists: "You have unsaved changes." with
Discard, when the page can offer it, and then Save — the primary action last, in
reading and tab order, as `Ui.Form.unsavedBar` asks. Render it in the page
template's footer slot.

It stays up while the save is in flight even though the form no longer differs
from what is being saved, so the button the reader pressed does not vanish from
under them. Once the save succeeded, with nothing changed since, Save shows a
check while the bar goes, so the press gets an answer.

Save is a plain button, not a submit — the bar is outside the `form` — so pass
the same message the form's `onSubmit` sends. It is still the form's submit to a
test, so its test hook is `form-submit`, as `Button.submit`'s is; Discard's is
`form-discard`.

-}
unsavedBar :
    WithLocale a
    ->
        { changed : Bool
        , result : ActionResult x
        , onSave : msg
        , onDiscard : Maybe msg
        }
    -> Html msg
unsavedBar appState config =
    let
        saving =
            ActionResult.isLoading config.result

        -- Saved, and nothing changed since: the bar goes, its Save showing the
        -- check on the way out. A page that keeps the bar up after a save
        -- (`changed` stays true until it moves on) shows a plain Save instead.
        saved =
            not config.changed && ActionResult.isSuccess config.result

        discard =
            case config.onDiscard of
                Just msg ->
                    [ Button.ghost (gettext "Discard" appState.locale) msg
                        |> Button.withDisabled saving
                        |> Button.withTestId "form-discard"
                    ]

                Nothing ->
                    []
    in
    Ui.Form.unsavedBar
        { visible = config.changed || saving
        , message = gettext "You have unsaved changes." appState.locale
        , actions =
            discard
                ++ [ Button.primary (gettext "Save" appState.locale) config.onSave
                        |> Button.withLoading saving
                        |> Button.withDone saved
                        |> Button.withTestId "form-submit"
                   ]
        }


{-| The reload after a save that needs the application loaded again. It waits
for the unsaved bar to show the check on its Save and go — the bar holds for
800ms and fades for 200ms — so the press gets its answer there too. Such a page
passes `changed` as false once the save succeeded, so the bar goes.
-}
reloadWhenSaved : Cmd msg
reloadWhenSaved =
    Window.refreshAfter 1000


{-| After a failed request, move focus to the server error. It waits for the
summary to be rendered, so call it from the same update that stores the error.
-}
focusErrorSummary : Cmd msg
focusErrorSummary =
    Dom.focus ("#" ++ errorSummaryId)
