module Ui.Link exposing
    ( Link
    , action
    , actionWithDescription
    , actionWithTestId
    , danger
    , external
    , link
    , toHtml
    , withExternalNotice
    , withIcon
    , withTestId
    )

{-| An inline text link: words you follow, in a sentence or under a form.

A link that should look like a button is `Ui.Button.link`, `linkPrimary` or
`linkGhost`; this is the one that looks like text. Both render `a` with a real
`href`, because navigation has to survive middle-click, copy link and the
screen reader's list of links, none of which a click handler provides.

The link is always underlined. Its color is well short of 3:1 against the
text around it, so without the line it would be told apart by hue alone
(WCAG 1.4.1). It takes no minimum hit target: 2.5.8 exempts a target whose size
is set by the line of text it sits in.

    Link.link { label = "Forgotten your password?", url = "/forgotten-password" }
        |> Link.toHtml

-}

import Html exposing (Html, a, button, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, rel, target, type_)
import Html.Events exposing (onClick)
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId
import Ui.Tooltip as Tooltip


type Link
    = Link Config


type alias Config =
    { label : String
    , url : String
    , external : Bool
    , externalNotice : String
    , danger : Bool
    , icon : Maybe Icon
    , testId : Maybe String
    }


link : { label : String, url : String } -> Link
link config =
    Link
        { label = config.label
        , url = config.url
        , external = False
        , externalNotice = "(opens in a new tab)"
        , danger = False
        , icon = Nothing
        , testId = Nothing
        }


{-| Opens in a new tab, and says so.

A new tab is a change of context nobody asked for (WCAG 3.2.5), so it is
announced twice: a trailing icon for the eye, and a visually hidden
"(opens in a new tab)" at the end of the accessible name. `rel="noopener
noreferrer"` stops the opened page reaching back through `window.opener`.

-}
external : Link -> Link
external (Link config) =
    Link { config | external = True }


{-| The words `external` adds, in the language of the page. Defaults to English.
-}
withExternalNotice : String -> Link -> Link
withExternalNotice notice (Link config) =
    Link { config | externalNotice = notice }


{-| A link to something destructive — "Delete this account". The color is the
danger role; the words have to say what happens, because a color cannot.
-}
danger : Link -> Link
danger (Link config) =
    Link { config | danger = True }


{-| A leading icon. Decorative: the label is the name.
-}
withIcon : Icon -> Link -> Link
withIcon icon (Link config) =
    Link { config | icon = Just icon }


{-| A `data-cy` attribute for the end-to-end tests. The one attribute escape
hatch, named, as `Ui.Button.withTestId` is.
-}
withTestId : String -> Link -> Link
withTestId testId (Link config) =
    Link { config | testId = Just testId }


{-| Words that look like a link and open something on the page instead of going
somewhere: an entry's title in a list whose detail opens beside it. It is a
`button`, because it has no address to follow — no middle-click, no copy link —
and it carries an `id`, so whatever it opened can hand focus back to it when it
closes.

Use it where the neighboring entries of other lists are links, so a title looks
like a title everywhere. A control that stands on its own is `Ui.Button`.

-}
action : { id : String, label : String, onClick : msg } -> Html msg
action config =
    button
        [ class "ui-link ui-link--action"
        , type_ "button"
        , id config.id
        , onClick config.onClick
        ]
        [ text config.label ]


{-| `action` with a test hook on the button.
-}
actionWithTestId : String -> { id : String, label : String, onClick : msg } -> Html msg
actionWithTestId testId config =
    button
        (class "ui-link ui-link--action"
            :: type_ "button"
            :: id config.id
            :: onClick config.onClick
            :: TestId.attributes (Just testId)
        )
        [ text config.label ]


{-| `actionWithTestId` with a tooltip saying what the click does, where the label
alone does not: a file's name that downloads it. The tooltip describes the button
(`aria-describedby`), so a screen reader says it too.
-}
actionWithDescription : String -> { id : String, label : String, onClick : msg, description : String } -> Html msg
actionWithDescription testId config =
    let
        descriptionId =
            config.id ++ "-description"
    in
    Tooltip.viewDescribing { id = descriptionId, label = config.description, placement = Tooltip.Below }
        (button
            (class "ui-link ui-link--action"
                :: type_ "button"
                :: id config.id
                :: attribute "aria-describedby" descriptionId
                :: onClick config.onClick
                :: TestId.attributes (Just testId)
            )
            [ text config.label ]
        )


toHtml : Link -> Html msg
toHtml (Link config) =
    let
        targetAttrs =
            if config.external then
                [ target "_blank", rel "noopener noreferrer" ]

            else
                []

        iconHtml =
            case config.icon of
                Just icon ->
                    span [ class "ui-link__icon" ] [ Icon.toHtml icon ]

                Nothing ->
                    text ""

        externalHtml =
            if config.external then
                span [ class "ui-link__external" ]
                    [ Icon.toHtml (Icon.solid "fa-arrow-up-right-from-square")
                    , span [ class "ui-visually-hidden" ] [ text (" " ++ config.externalNotice) ]
                    ]

            else
                text ""
    in
    a
        (classList [ ( "ui-link", True ), ( "ui-link--danger", config.danger ) ]
            :: href config.url
            :: targetAttrs
            ++ TestId.attributes config.testId
        )
        [ iconHtml, text config.label, externalHtml ]
