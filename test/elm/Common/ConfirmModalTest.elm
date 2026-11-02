module Common.ConfirmModalTest exposing (suite)

{-| Which confirm button a question gets.

The color of the button is the last thing read before something irreversible
happens, so which helper a caller reaches for decides whether the warning is
there at all. These pin the three down, because the difference is invisible at
the call site — every one of them is `ConfirmModal.<something> appState { … }`
with a label in it — and a destructive question that took the primary button
looks exactly like a correct one until it is pressed.

-}

import ActionResult
import Common.Components.ConfirmModal as ConfirmModal
import Expect
import Gettext
import Html
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class)
import Ui.Modal as Modal


type Msg
    = Confirmed
    | Closed


locale : { locale : Gettext.Locale }
locale =
    { locale = Gettext.defaultLocale }


deleteModal : Query.Single Msg
deleteModal =
    Query.fromHtml
        (ConfirmModal.delete locale
            { title = "Delete the thing"
            , open = True
            , question = "Are you sure?"
            , entity = Html.text "the thing"
            , onConfirm = Confirmed
            , onClose = Closed
            , result = ActionResult.Unset
            }
        )


dangerModal : Query.Single Msg
dangerModal =
    Query.fromHtml
        (ConfirmModal.dangerConfirm locale
            { title = "Discard changes"
            , open = True
            , body = [ Html.text "Everything since the last save goes." ]
            , confirmLabel = "Discard"
            , onConfirm = Confirmed
            , onClose = Closed
            , result = ActionResult.Unset
            , testId = Nothing
            }
        )


plainModal : Query.Single Msg
plainModal =
    Query.fromHtml
        (ConfirmModal.confirm locale
            { title = "Resend the invitation"
            , open = True
            , body = [ Html.text "It will be sent again." ]
            , confirmLabel = "Resend"
            , onConfirm = Confirmed
            , onClose = Closed
            , result = ActionResult.Unset
            }
        )


{-| `ui-button--danger-secondary` also starts with `ui-button--danger`, so the
count is of the class itself rather than of a prefix.
-}
danger : Query.Single Msg -> Int -> Expect.Expectation
danger modal expected =
    modal
        |> Query.findAll [ class "ui-button--danger" ]
        |> Query.count (Expect.equal expected)


suite : Test
suite =
    describe "Common.Components.ConfirmModal"
        [ test "deleting is confirmed by a danger button" <|
            \_ -> danger deleteModal 1
        , test "so is anything else destructive" <|
            \_ -> danger dangerModal 1
        , test "a question that destroys nothing keeps the primary button" <|
            \_ ->
                Expect.all
                    [ \modal -> danger modal 0
                    , Query.has [ class "ui-button--primary" ]
                    ]
                    plainModal
        , test "a form that destroys something submits in danger too" <|
            \_ -> danger destructiveForm 1
        , test "a form that does not keeps the primary submit" <|
            \_ ->
                Expect.all
                    [ \modal -> danger modal 0
                    , Query.has [ class "ui-button--primary" ]
                    ]
                    plainForm
        ]


{-| A dialog whose submission is the destruction — the package delete, which
asks for the name to be typed first. It is still a dialog asking whether to
destroy something, so its submit is red.
-}
destructiveForm : Query.Single Msg
destructiveForm =
    Query.fromHtml (formModal True)


plainForm : Query.Single Msg
plainForm =
    Query.fromHtml (formModal False)


formModal : Bool -> Html.Html Msg
formModal dangerous =
    Modal.plain
        { title = "Delete the package"
        , open = True
        , onClose = Closed
        , body = [ Html.text "Type its name to confirm." ]
        }
        |> Modal.withForm { onSubmit = Confirmed, submitLabel = "Delete" }
        |> Modal.withDangerConfirm dangerous
        |> Modal.toHtml
