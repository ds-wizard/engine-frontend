module Common.Components.ExternalLoginButton exposing (badge, badges, icon, preview, view, viewLoading)

{-| Signing in with an external identity provider — "Google", "ORCID".

It is a `Ui.Button.secondary`: an action, because signing in starts a request
rather than following a link, with the provider's icon before its name. The
name is the button's text and therefore its accessible name; the icon is
decorative.

**The provider's own colors are used exactly as configured.** Each provider
has a background and a text color. Either one left unset takes the default the
buttons have always had: a dark ground, `#333`, and white text. The values are
passed through untouched, in any CSS color syntax.

This is a deliberate exception to the rest of the system: **there is no contrast
check here.** A provider's button is often governed by that provider's brand
guidelines, which an instance may have to follow to the letter, so the
administrator who configures it owns its legibility. What the system still owns
is everything that makes it a button: a border (1.4.11 — a custom ground can
match the page, as a dark button does on the dark theme), the focus ring, and a
name that is always text.

The colors reach the stylesheet as custom properties on a `.c-external-login`
wrapper, not as an inline `background` on the button, so hover, focus and the
border stay the system's. A provider color does not remove the border, it tints
it: the border is a tone of the provider's ground mixed toward its text color,
so it is there when a custom background matches the page and the border is what
makes the button a button (1.4.11), without a gray edge around the brand. A busy
button's spinner is in the provider's text color.

Brand icons are Font Awesome's brands style (`fab fa-google`). The configured
class is used as given; an application that shows these has to load the brands
stylesheet.

-}

import Common.Api.Models.AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, style)
import Ui.Button as Button
import Ui.Icon as Icon
import Uuid exposing (Uuid)


view :
    { service : { a | uuid : Uuid, name : String, style : AuthServiceProviderButtonStyle }
    , onClick : msg
    }
    -> Html msg
view config =
    render [] False config.service config.service.name config.onClick


{-| `view`, busy while the sign-in it started is on its way: the spinner is in
the button, so nothing beside it appears and the row does not move.
-}
viewLoading :
    Bool
    ->
        { service : { a | uuid : Uuid, name : String, style : AuthServiceProviderButtonStyle }
        , onClick : msg
        }
    -> Html msg
viewLoading loading config =
    render [] loading config.service config.service.name config.onClick


{-| The button as `view` draws it, for a settings form to show what the login
screen will — and not a control.

A preview that is a real button is a tab stop that does nothing, announced as
something to press. It is `inert` instead: out of the tab order and out of the
accessibility tree. What it looks like is the only thing it says, so a caller
that puts it in a field says in words what it shows (the name) beside it.

An empty name would draw a button with no text, so `placeholderName` stands in
for it until the name is typed.

-}
preview :
    { service : { a | uuid : Uuid, name : String, style : AuthServiceProviderButtonStyle }
    , placeholderName : String
    , noop : msg
    }
    -> Html msg
preview config =
    let
        name =
            if String.isEmpty (String.trim config.service.name) then
                config.placeholderName

            else
                config.service.name
    in
    render [ attribute "inert" "" ] False config.service name config.noop


render :
    List (Html.Attribute msg)
    -> Bool
    -> { a | uuid : Uuid, name : String, style : AuthServiceProviderButtonStyle }
    -> String
    -> msg
    -> Html msg
render attributes loading service name onClick =
    let
        button =
            Button.secondary name onClick
                |> Button.withIcon (Icon.fromClass (Maybe.withDefault defaultIcon service.style.icon))
                |> Button.withTestId ("login_external_" ++ Uuid.toString service.uuid)
                |> Button.withLoading loading
                |> Button.toHtml

        buttonColors =
            colors service.style
    in
    div
        (class "c-external-login"
            :: style "--c-external-login-background" buttonColors.background
            :: style "--c-external-login-color" buttonColors.color
            :: attributes
        )
        [ button ]


{-| The provider's icon on its own ground, in its configured colors — the small
mark a list of providers shows beside each name, so the list looks like the
buttons it configures.

Decorative (`aria-hidden`): the provider's name is always next to it, and it is
what the row is called.

-}
icon : AuthServiceProviderButtonStyle -> Html msg
icon buttonStyle =
    let
        iconColors =
            colors buttonStyle
    in
    span
        [ class "c-external-login-icon"
        , style "--c-external-login-background" iconColors.background
        , style "--c-external-login-color" iconColors.color
        , attribute "aria-hidden" "true"
        ]
        [ Icon.toHtml (Icon.fromClass (Maybe.withDefault defaultIcon buttonStyle.icon)) ]


{-| The provider as a badge — its icon and name in its configured colors — for
a line that says which providers an account is connected to, as a list of users
does on each row.

Not a control: the name is the text, the icon decorative.

-}
badge : { a | name : String, style : AuthServiceProviderButtonStyle } -> Html msg
badge service =
    let
        badgeColors =
            colors service.style
    in
    span
        [ class "c-external-login-badge"
        , style "--c-external-login-background" badgeColors.background
        , style "--c-external-login-color" badgeColors.color
        ]
        [ span [ attribute "aria-hidden" "true" ]
            [ Icon.toHtml (Icon.fromClass (Maybe.withDefault defaultIcon service.style.icon)) ]
        , text service.name
        ]


{-| A run of `badge`s on one line, inline so it can sit in running text or on
a list row's line of fragments.
-}
badges : List { a | name : String, style : AuthServiceProviderButtonStyle } -> Html msg
badges services =
    span [ class "c-external-login-badges" ] (List.map badge services)


defaultIcon : String
defaultIcon =
    "fab fa-openid"


{-| The colors the button is drawn with: the provider's, with the defaults for
any it does not set.
-}
colors : AuthServiceProviderButtonStyle -> { background : String, color : String }
colors buttonStyle =
    { background = Maybe.withDefault defaultBackground buttonStyle.background
    , color = Maybe.withDefault defaultColor buttonStyle.color
    }


defaultBackground : String
defaultBackground =
    "#333333"


defaultColor : String
defaultColor =
    "#ffffff"
