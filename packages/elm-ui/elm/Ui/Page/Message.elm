module Ui.Page.Message exposing
    ( Message
    , announced
    , message
    , messageHtml
    , toHtml
    , withAction
    , withDetails
    , withIllustration
    , withinPage
    )

{-| A full-page illustrated message: errors, not found, access denied, and the
empty state of a list.

This is one of the few templates where expressive treatment is allowed. Making
that a property of _which template a screen uses_ rather than of taste is what
makes the rule survive a deadline.

Illustrations are fixed artwork with their colors baked in — they do not follow
the theme, so on a dark ground they need their own asset rather than a token.

The message is the page, so its title is the page's `h1` (`Ui.Text.pageDisplay`)
and where focus lands after a route change. `withinPage` is for the same message
filling a region of a page that already has one.

-}

import Html exposing (Html, div, h2, p, section, text)
import Html.Attributes exposing (attribute, class, classList)
import Maybe.Extra as Maybe
import Ui.Button as Button exposing (Button)
import Ui.Layout as Layout
import Ui.Text as Text


type Message msg
    = Message
        { title : String
        , description : List (Html msg)
        , illustration : Maybe (Html msg)
        , action : Maybe (Button msg)
        , details : Maybe (Html msg)
        , within : Bool
        , announced : Bool
        }


message : { title : String, description : String } -> Message msg
message config =
    messageHtml { title = config.title, description = [ text config.description ] }


{-| `message`, with a sentence that marks up part of itself — a name in bold, an
identifier in code (`Ui.Text.code`).
-}
messageHtml : { title : String, description : List (Html msg) } -> Message msg
messageHtml config =
    Message
        { title = config.title
        , description = config.description
        , illustration = Nothing
        , action = Nothing
        , details = Nothing
        , within = False
        , announced = False
        }


withIllustration : Html msg -> Message msg -> Message msg
withIllustration illustration (Message config) =
    Message { config | illustration = Just illustration }


withAction : Button msg -> Message msg -> Message msg
withAction action (Message config) =
    Message { config | action = Just action }


{-| What the sentence cannot hold — the log of what failed, in a
`Ui.CodeBlock`. It comes last, after the action, and is the one part that is not
centered: a log is read from the start of its lines. The message grows wider to
give it room.
-}
withDetails : Html msg -> Message msg -> Message msg
withDetails details (Message config) =
    Message { config | details = Just details }


{-| The message as a region of a page that already has a heading — the tab of a
detail page that has nothing to show, or could not show it.

It looks the same: the illustration, the title at the display size, the
sentence and the action. What changes is what the title is. The `h1` belongs to
whatever contains this, so the title is an `h2` without
`Ui.AppShell.mainHeadingId` and does not take the shell's focus on a route
change — the page's own heading already did. Like `Ui.Page.Content.withinPage`
it has no scroll container of its own; it is centered in whatever height the
containing template gives it.

-}
withinPage : Message msg -> Message msg
withinPage (Message config) =
    Message { config | within = True }


{-| The sentence is said as it appears: an error that arrives while the user is
already on the page, where nothing else would tell a screen reader that the
region under the tabs changed.

Only the sentence is the alert. The title stays a heading in the outline rather
than a heading inside an alert, the action is not read out as part of the
announcement, and the sentence is the part that says what happened — the title
of an error is usually just "Error". A message that is the whole page after a
route change does not need it: the route change already moved focus to its
title and announced it.

-}
announced : Message msg -> Message msg
announced (Message config) =
    Message { config | announced = True }


toHtml : Message msg -> Html msg
toHtml (Message config) =
    section
        [ classList
            [ ( "ui-page-message", True )
            , ( "ui-page-message--within", config.within )
            , ( "ui-page-message--details", Maybe.isJust config.details )
            ]
        ]
        [ case config.illustration of
            Just illustration ->
                div [ class "ui-page-message__illustration" ] [ illustration ]

            Nothing ->
                Html.text ""
        , Layout.stack
            [ if config.within then
                h2 [ class "ui-display" ] [ text config.title ]

              else
                Text.pageDisplay config.title
            , if config.announced then
                p [ class "ui-body", attribute "role" "alert" ] config.description

              else
                Text.bodyHtml config.description
            , case config.action of
                Just action ->
                    div [] [ Button.toHtml action ]

                Nothing ->
                    Html.text ""
            ]
        , case config.details of
            Just details ->
                div [ class "ui-page-message__details" ] [ details ]

            Nothing ->
                Html.text ""
        ]
