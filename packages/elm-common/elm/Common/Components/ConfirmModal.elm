module Common.Components.ConfirmModal exposing
    ( confirm
    , confirmWithTestId
    , dangerConfirm
    , delete
    , deleteWithTestId
    )

{-| A question that has to be answered before something happens, with the
answer's result shown where it was asked.

Both are `Ui.Modal.confirm`: the native dialog, so focus goes in on opening,
stays in while open and returns to the trigger on closing without anything here
arranging it. The confirm button shows the request in flight with
`withLoading`, which keeps it disabled — a second press cannot send a second
request.

**A failure is shown inside the dialog**, not behind it. The request failed
because of the thing the dialog is about, and a message on the page under a
modal backdrop is a message nobody can read. The error is a `Ui.Alert.danger`,
so it is announced when it appears, and the dialog stays open so the user can
try again or cancel.

**The error has a place in the body whether or not there is an error.**
`ui-dialog` moves its children into the dialog when it connects, so the body is
built once for the life of the modal; the slot for the error is always there
and only its contents change. That keeps the rule `Ui.Modal` sets — a modal
never gains or loses child elements while it is mounted — without the caller
having to know about it.

-}

import ActionResult exposing (ActionResult(..))
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Ui.Alert as Alert
import Ui.Modal as Modal
import Ui.Text as Text


{-| A question whose confirmation is **not** destructive — "Resend the
invitation?", "Clone this project?", "Refresh the page?". The confirm button is
primary.

If answering it destroys anything — deletes, discards, revokes, disconnects,
reverts — this is the wrong one: use `delete` or `dangerConfirm`, so the color
of the button says what pressing it does. The two read identically at the call
site, which is why `test/elm/Common/ConfirmModalTest.elm` pins them down.

-}
confirm :
    WithLocale a
    ->
        { title : String
        , open : Bool
        , body : List (Html msg)
        , confirmLabel : String
        , onConfirm : msg
        , onClose : msg
        , result : ActionResult x
        }
    -> Html msg
confirm appState config =
    confirmWithTestId appState
        { title = config.title
        , open = config.open
        , body = config.body
        , confirmLabel = config.confirmLabel
        , onConfirm = config.onConfirm
        , onClose = config.onClose
        , result = config.result
        , testId = Nothing
        }


{-| `confirm`, with a `data-cy` on the dialog for the end-to-end tests.
-}
confirmWithTestId :
    WithLocale a
    ->
        { title : String
        , open : Bool
        , body : List (Html msg)
        , confirmLabel : String
        , onConfirm : msg
        , onClose : msg
        , result : ActionResult x
        , testId : Maybe String
        }
    -> Html msg
confirmWithTestId appState config =
    Modal.confirm
        { title = config.title
        , open = config.open
        , onClose = config.onClose
        , body = config.body ++ [ errorSlot config.result ]
        , confirmLabel = config.confirmLabel
        , onConfirm = config.onConfirm
        }
        |> Modal.withLoading (ActionResult.isLoading config.result)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> withMaybeTestId config.testId
        |> Modal.toHtml


{-| Deleting something. The question says what happens — "Are you sure you want
to permanently delete this project?" — and `entity` shows which one, so the
decision is made about a thing the user can see rather than about a name they
have to remember. The confirm button is the danger variant and says "Delete".
-}
delete :
    WithLocale a
    ->
        { title : String
        , open : Bool
        , question : String
        , entity : Html msg
        , onConfirm : msg
        , onClose : msg
        , result : ActionResult x
        }
    -> Html msg
delete appState config =
    deleteWithTestId appState
        { title = config.title
        , open = config.open
        , question = config.question
        , entity = config.entity
        , onConfirm = config.onConfirm
        , onClose = config.onClose
        , result = config.result
        , testId = Nothing
        }


{-| `delete`, with a `data-cy` on the dialog for the end-to-end tests.
-}
deleteWithTestId :
    WithLocale a
    ->
        { title : String
        , open : Bool
        , question : String
        , entity : Html msg
        , onConfirm : msg
        , onClose : msg
        , result : ActionResult x
        , testId : Maybe String
        }
    -> Html msg
deleteWithTestId appState config =
    dangerConfirm appState
        { title = config.title
        , open = config.open
        , body = [ Text.body config.question, config.entity ]
        , confirmLabel = gettext "Delete" appState.locale
        , onConfirm = config.onConfirm
        , onClose = config.onClose
        , result = config.result
        , testId = config.testId
        }


{-| A destructive question whose answer is not "Delete" — "Revoke",
"Disconnect", "Discard changes". The confirm button is the danger variant and
says `confirmLabel`, which should be the verb for what happens, never "OK" or
"Yes": a button that says what it does can be pressed without rereading the
question.

`testId` is a `data-cy` on the dialog, for the end-to-end tests.

-}
dangerConfirm :
    WithLocale a
    ->
        { title : String
        , open : Bool
        , body : List (Html msg)
        , confirmLabel : String
        , onConfirm : msg
        , onClose : msg
        , result : ActionResult x
        , testId : Maybe String
        }
    -> Html msg
dangerConfirm appState config =
    Modal.confirm
        { title = config.title
        , open = config.open
        , onClose = config.onClose
        , body = config.body ++ [ errorSlot config.result ]
        , confirmLabel = config.confirmLabel
        , onConfirm = config.onConfirm
        }
        |> Modal.withDangerConfirm True
        |> Modal.withLoading (ActionResult.isLoading config.result)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> withMaybeTestId config.testId
        |> Modal.toHtml


withMaybeTestId : Maybe String -> Modal.Modal msg -> Modal.Modal msg
withMaybeTestId maybeTestId modal =
    case maybeTestId of
        Just testId ->
            Modal.withTestId testId modal

        Nothing ->
            modal


errorSlot : ActionResult x -> Html msg
errorSlot result =
    div []
        [ case result of
            Error message ->
                Alert.toHtml (Alert.danger message)

            _ ->
                Html.text ""
        ]
