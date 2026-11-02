module Common.Components.ReportIssueModal exposing (view)

{-| Where to report a problem: the support site and the support address.

It has no state of its own beyond being open, so it keeps none — the
application holds the flag. The values are plain strings rather than a
configuration type, because each application reads them from a differently
shaped configuration and this is the same dialog in all of them.

Both destinations are links, never buttons: the site opens in a new tab and
says so, and the address is a `mailto:` link, which is what lets a mail client,
a copy-link or a screen reader's links list do something with it.

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html, p)
import String.Format as String
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Link as Link
import Ui.Modal as Modal
import Ui.Text as Text


view :
    WithLocale a
    ->
        { open : Bool
        , onClose : msg
        , supportSiteName : String
        , supportSiteUrl : String
        , supportSiteIcon : String
        , supportEmail : String
        }
    -> Html msg
view appState config =
    let
        supportSiteLink =
            Link.link { label = config.supportSiteName, url = config.supportSiteUrl }
                |> Link.withIcon (Icon.fromClass config.supportSiteIcon)
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml

        supportMailLink =
            Link.link { label = config.supportEmail, url = "mailto:" ++ config.supportEmail }
                |> Link.toHtml
    in
    Modal.plain
        { title = gettext "Report Issue" appState.locale
        , open = config.open
        , onClose = config.onClose
        , body =
            [ Text.body (gettext "If you find any problem, you can report an issue here:" appState.locale)
            , p [] [ supportSiteLink ]
            , p [] (String.formatHtml (gettext "You can also write us an email to %s." appState.locale) [ supportMailLink ])
            ]
        }
        |> Modal.withActions [ Button.primary (gettext "OK" appState.locale) config.onClose ]
        |> Modal.withTestId "report-issue"
        |> Modal.toHtml
