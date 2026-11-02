module Ui.Banner exposing
    ( Banner
    , info
    , toHtml
    , warning
    , withActions
    , withDismiss
    , withHtmlContent
    , withLabel
    )

{-| A page-level notice: cookie consent, an announcement, a session about to
expire. It is about the whole application, not about a region of the page —
that is `Ui.Alert`.

**Render it after the main content, in DOM order.** The banner is pinned to the
block end of whatever scrolls it with `position: sticky`, so it is seen first
without being reached first. A cookie notice as the first tab stop puts itself
between every keyboard user and the skip link on every page until it is
answered, and moving it up the page with CSS instead would break the rule that
DOM order is focus order. A pinned bar can cover a focused
element as the page scrolls under it, so give the scroll container a
`scroll-padding-block-end` as tall as the banner (2.4.11).

It is a `section` named by `withLabel`, which makes it a region landmark a
screen reader can jump to. Without a label it is not a landmark at all — give it
one. It is not a live region: a notice that is there when the page loads is read
where it is. One that appears while somebody is working and needs their
attention now, like a session about to end, should also say so through a status
message of the page's own.

The tone is an icon and a colored edge on a raised ground, never a tint alone
(1.4.1). The ground is the elevated surface rather than the status tint so that
buttons and links inside it sit on a pairing `Ui.Theme.Check` already verifies.

**Dismissing moves focus, and that is the caller's job.** The dismiss button
is removed with the banner, so focus would fall to the document. After handling
`onDismiss`, focus something meaningful — the main content's heading, or
whatever the notice was about.

-}

import Html exposing (Html, div, p, section, text)
import Html.Attributes exposing (attribute, class)
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon


type Banner msg
    = Banner (Config msg)


type alias Config msg =
    { tone : Tone
    , content : List (Html msg)
    , actions : List (Button msg)
    , dismiss : Maybe { label : String, onDismiss : msg }
    , label : Maybe String
    }


type Tone
    = Info
    | Warning


base : Tone -> String -> Banner msg
base tone message =
    Banner
        { tone = tone
        , content = [ p [] [ text message ] ]
        , actions = []
        , dismiss = Nothing
        , label = Nothing
        }


info : String -> Banner msg
info =
    base Info


warning : String -> Banner msg
warning =
    base Warning


{-| Replaces the text with markup — for a sentence with a `Ui.Link` in it.
Keep it to phrasing and paragraphs; a banner holding a heading or a form is a
page.
-}
withHtmlContent : List (Html msg) -> Banner msg -> Banner msg
withHtmlContent content (Banner config) =
    Banner { config | content = content }


{-| The banner's answers — "Accept", "Reject", "Stay signed in". They follow
the text in DOM order and wrap below it when there is no room beside it.
-}
withActions : List (Button msg) -> Banner msg -> Banner msg
withActions actions (Banner config) =
    Banner { config | actions = actions }


{-| An icon-only close button at the end. The label is required, because it is
the button's only name. See the module documentation for where focus goes
next.
-}
withDismiss : { label : String, onDismiss : msg } -> Banner msg -> Banner msg
withDismiss dismiss (Banner config) =
    Banner { config | dismiss = Just dismiss }


{-| The region's name — "Cookie consent", "Announcement".
-}
withLabel : String -> Banner msg -> Banner msg
withLabel label (Banner config) =
    Banner { config | label = Just label }


toHtml : Banner msg -> Html msg
toHtml (Banner config) =
    let
        ( modifier, icon ) =
            case config.tone of
                Info ->
                    ( "ui-banner--info", "fa-circle-info" )

                Warning ->
                    ( "ui-banner--warning", "fa-triangle-exclamation" )

        labelAttrs =
            case config.label of
                Just label ->
                    [ attribute "aria-label" label ]

                Nothing ->
                    []

        actions =
            if List.isEmpty config.actions then
                text ""

            else
                div [ class "ui-banner__actions" ] (List.map Button.toHtml config.actions)

        dismiss =
            case config.dismiss of
                Just { label, onDismiss } ->
                    div [ class "ui-banner__dismiss" ]
                        [ Button.toHtml (Button.iconOnly (Icon.solid "fa-xmark") label onDismiss) ]

                Nothing ->
                    text ""
    in
    section (class ("ui-banner " ++ modifier) :: labelAttrs)
        [ div [ class "ui-banner__inner" ]
            [ div [ class "ui-banner__icon" ] [ Icon.toHtml (Icon.solid icon) ]
            , div [ class "ui-banner__content" ] config.content
            , actions
            , dismiss
            ]
        ]
