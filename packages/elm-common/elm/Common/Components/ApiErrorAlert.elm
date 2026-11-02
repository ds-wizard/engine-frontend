module Common.Components.ApiErrorAlert exposing
    ( focus
    , id
    , view
    )

{-| The error from a request that failed, where the request was made.

It is not a message at the top of the page, whatever had failed and wherever
the user was looking. Rendered next to the thing that failed — above a form's actions, inside a modal — it is read in
context.

`Ui.Alert.danger` is `role="alert"`, so the message is announced when it
appears. It sits in a container with a stable `id` and `tabindex -1` as well, so
that after a failed submit the page can move focus to it with `focus`: a
keyboard user who pressed Save at the bottom of a long form is otherwise left
on the button with the explanation somewhere above them, off the screen. The
container is not in the tab order — `-1` only makes it a target for focus
moved by the page.

There is one per page. The id is fixed so `focus` needs nothing but a message,
and a page with two failing requests has one place to say so.

**It is the form's error summary.** `Common.Components.Form.errorSummary` is
the same element with the same id, rendered by one function, so a page can
focus either with either module's `focus` — a server error is a server error
whether or not a form sent the request.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Components.Form as Form
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import Task


id : String
id =
    Form.errorSummaryId


{-| The error, when there is one; nothing otherwise. The message is already
translated — it is the one the page gave `ApiError.toActionResult`. An empty
message is said as "Error": an alert that announces nothing is worse than a
vague one.
-}
view : WithLocale a -> ActionResult x -> Html msg
view appState result =
    case result of
        Error message ->
            if String.isEmpty (String.trim message) then
                Form.errorSummary (Error (gettext "Error" appState.locale))

            else
                Form.errorSummary result

        _ ->
            Html.text ""


{-| Moves focus to the error. `Browser.Dom` waits for the next render, so this
can be returned from the same `update` that stored the error. The message is
sent whether or not the element was found.
-}
focus : msg -> Cmd msg
focus msg =
    Browser.Dom.focus id
        |> Task.attempt (always msg)
