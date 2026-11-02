module Ui.AsyncBlock exposing (view)

{-| The loading / error / loaded pattern, in one place.

`ActionResultBlock` exists in `common`, `ActionResultView` in wizard and
`ActionResultIcon` in internal — the same idea written three times. This takes
the state as a plain value rather than depending on `ActionResult`, so the
design system does not acquire an opinion about how an app models its requests.

The loading state announces itself politely: a spinner that says nothing is
invisible to a screen reader, and a silent page after an action is the most
common way an accessible app still feels broken.

-}

import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class)
import Ui.Alert as Alert
import Ui.Icon as Icon
import Ui.Text as Text


view :
    { loading : Bool
    , error : Maybe String
    , loadingLabel : String
    , content : List (Html msg)
    }
    -> Html msg
view config =
    if config.loading then
        div
            [ class "ui-async ui-async--loading"
            , attribute "role" "status"
            , attribute "aria-live" "polite"
            ]
            [ Icon.toHtml Icon.spinner
            , Text.muted config.loadingLabel
            ]

    else
        case config.error of
            Just message ->
                div [ class "ui-async ui-async--error" ]
                    [ Alert.toHtml (Alert.danger message) ]

            Nothing ->
                div [ class "ui-async" ] config.content
