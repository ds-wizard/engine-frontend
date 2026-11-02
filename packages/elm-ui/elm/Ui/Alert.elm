module Ui.Alert exposing
    ( Alert
    , danger
    , info
    , private
    , success
    , toHtml
    , toHtmlWithContent
    , warning
    , withIcon
    )

{-| Status is carried by an icon and a border as well as by color, so the alert
never depends on hue alone (WCAG 1.4.1). Danger and warning announce themselves
to assistive technology; success and info do not interrupt.
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class)
import Ui.Icon as Icon


type Alert
    = Alert
        { modifier : String
        , icon : Icon.Icon
        , message : String
        , assertive : Bool
        }


{-| A different icon for the same tone — a lock on the note that says who can
see something. The tone still decides the color, the border and whether the
alert interrupts; only the picture changes.
-}
withIcon : Icon.Icon -> Alert -> Alert
withIcon icon (Alert config) =
    Alert { config | icon = icon }


success : String -> Alert
success message =
    Alert { modifier = "ui-alert--success", icon = Icon.solid "fa-circle-check", message = message, assertive = False }


info : String -> Alert
info message =
    Alert { modifier = "", icon = Icon.solid "fa-circle-info", message = message, assertive = False }


{-| What only some of the people looking at a thing can see — an editor's note
on a project everyone else can read.

Not a status: nothing has gone right or wrong, and it does not interrupt. The
lock is the picture because that is what being private looks like, and the hue
is the one the rest of the private furniture wears.

-}
private : String -> Alert
private message =
    Alert { modifier = "ui-alert--private", icon = Icon.solid "fa-lock", message = message, assertive = False }


warning : String -> Alert
warning message =
    Alert { modifier = "ui-alert--warning", icon = Icon.solid "fa-triangle-exclamation", message = message, assertive = True }


danger : String -> Alert
danger message =
    Alert { modifier = "ui-alert--danger", icon = Icon.solid "fa-circle-exclamation", message = message, assertive = True }


toHtml : Alert -> Html msg
toHtml ((Alert config) as alert) =
    view alert [ text config.message ]


{-| The alert with markup in place of its message — an announcement whose text
is rendered markdown, with a link or an emphasis in it.

The tone, the icon and the role are still the constructor's, so a critical
announcement is as assertive as any other danger alert; only the words become
markup. Keep it to phrasing and paragraphs: an alert holding a heading or a
form is a section of the page, not an alert. The constructor's message is
unused.

    Alert.info ""
        |> Alert.toHtmlWithContent [ renderedMarkdown ]

-}
toHtmlWithContent : List (Html msg) -> Alert -> Html msg
toHtmlWithContent content alert =
    view alert content


view : Alert -> List (Html msg) -> Html msg
view (Alert config) content =
    div
        [ class ("ui-alert " ++ config.modifier)
        , attribute "role"
            (if config.assertive then
                "alert"

             else
                "status"
            )
        ]
        [ div [ class "ui-alert__icon" ] [ Icon.toHtml config.icon ]
        , div [ class "ui-alert__content" ] content
        ]
