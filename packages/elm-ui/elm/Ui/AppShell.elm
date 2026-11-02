module Ui.AppShell exposing
    ( FocusRetry
    , Shell
    , Words
    , focusFragment
    , focusOnRouteChange
    , focusOnRouteChangeWhenReady
    , mainHeadingId
    , retryFocusOnRouteChange
    , shell
    , skipTargets
    , toHtml
    , withHeader
    , withSidePanel
    , withWords
    )

{-| The page frame: landmarks, skip links, and focus on route change.

Elm applications get the last of these wrong by default. Navigation replaces the
DOM without touching focus, so a keyboard user stays wherever they were and a
screen reader announces nothing at all. `focusOnRouteChange` is the fix, and
because every app uses the same shell, every app gets it.

The heading is focused with `.ui-focus-forced` rather than relying on
`:focus-visible`, which keys off the last input modality: after a mouse click on
a nav link the ring would otherwise never paint, and focus would move silently.

-}

import Browser.Dom as Dom
import Html exposing (Html, a, div, header, main_, nav, text)
import Html.Attributes exposing (class, href, id, tabindex)
import List.Extra as List
import Process
import Task


type Shell msg
    = Shell
        { navigation : Html msg
        , header : Maybe (Html msg)
        , sidePanel : Maybe (Html msg)
        , content : List (Html msg)
        , words : Words
        }


{-| What the skip links say. English unless `withWords` says otherwise.
-}
type alias Words =
    { skipToContent : String
    , skipToNavigation : String
    }


shell : { navigation : Html msg, content : List (Html msg) } -> Shell msg
shell config =
    Shell
        { navigation = config.navigation
        , header = Nothing
        , sidePanel = Nothing
        , content = config.content
        , words = { skipToContent = "Skip to content", skipToNavigation = "Skip navigation" }
        }


withHeader : Html msg -> Shell msg -> Shell msg
withHeader content (Shell config) =
    Shell { config | header = Just content }


{-| A panel down the end of the frame, beside the page — a `Ui.Drawer`. Pass it
only while it is open: the column is there only when the slot is filled, so a
frame with the panel closed is the frame without one.
-}
withSidePanel : Html msg -> Shell msg -> Shell msg
withSidePanel content (Shell config) =
    Shell { config | sidePanel = Just content }


{-| The skip links in the language of the page.
-}
withWords : Words -> Shell msg -> Shell msg
withWords words (Shell config) =
    Shell { config | words = words }


{-| The id the shell gives its main heading, so a page can point at it.
-}
mainHeadingId : String
mainHeadingId =
    "ui-main-heading"


{-| The ids the skip links point at.

An application intercepts its own in-page links, so a fragment never scrolls or
focuses by itself: the app has to notice a URL with a fragment and focus the
matching element. `focusFragment` is that, and without calling it the skip links
are decoration.

-}
skipTargets : List String
skipTargets =
    [ "ui-main", "ui-nav" ]


{-| Focus whatever a fragment names. Call it from the application's URL handling
so `#ui-main` actually moves focus rather than only changing the address.
-}
focusFragment : msg -> Maybe String -> Cmd msg
focusFragment noop fragment =
    case fragment of
        Just target ->
            Task.attempt (always noop) (Dom.focus target)

        Nothing ->
            Cmd.none


{-| Run this on every route change. Focus moves to the new view's heading, which
both announces the page and puts the keyboard somewhere sensible.
-}
focusOnRouteChange : msg -> Cmd msg
focusOnRouteChange noop =
    Task.attempt (always noop) (Dom.focus mainHeadingId)


{-| Where a `focusOnRouteChangeWhenReady` has got to: which attempt comes next.
Opaque, so the schedule stays the shell's.
-}
type FocusRetry
    = FocusRetry Int


{-| `focusOnRouteChange` for a page that may not have its heading yet.

A page that is still fetching shows a loading state, and a loading state has no
heading: focusing it right after the route changes finds nothing, and focus
falls to the document. This tries at once and, while the heading is missing,
asks again after a growing delay — about five seconds in all — then gives up
and sends `onDone`.

The retries go through the application's `update` rather than running as one
task chain, so an application can drop them: a second navigation before the
first page loaded must not let the first one's retry pull focus away from
wherever the user has gone since. Tag `onRetry` with something that identifies
the navigation, and call `retryFocusOnRouteChange` only if it is still current.

Because the heading is focused the first time it exists, the page must render
that heading once and keep it — a loading state with a heading of its own that
is then replaced by another element would drop focus again.

-}
focusOnRouteChangeWhenReady : { onRetry : FocusRetry -> msg, onDone : msg } -> Cmd msg
focusOnRouteChangeWhenReady config =
    attemptFocus config 0 (Dom.focus mainHeadingId)


{-| The next attempt of `focusOnRouteChangeWhenReady`.
-}
retryFocusOnRouteChange : { onRetry : FocusRetry -> msg, onDone : msg } -> FocusRetry -> Cmd msg
retryFocusOnRouteChange config (FocusRetry attempt) =
    case List.getAt attempt focusRetryDelays of
        Just delay ->
            attemptFocus config (attempt + 1) (Process.sleep delay |> Task.andThen (\_ -> Dom.focus mainHeadingId))

        Nothing ->
            Task.perform (always config.onDone) (Task.succeed ())


attemptFocus : { onRetry : FocusRetry -> msg, onDone : msg } -> Int -> Task.Task Dom.Error () -> Cmd msg
attemptFocus config nextAttempt task =
    Task.attempt
        (\result ->
            case result of
                Ok _ ->
                    config.onDone

                Err _ ->
                    if nextAttempt < List.length focusRetryDelays then
                        config.onRetry (FocusRetry nextAttempt)

                    else
                        config.onDone
        )
        task


{-| Milliseconds before each retry; 5.15 seconds in all.
-}
focusRetryDelays : List Float
focusRetryDelays =
    [ 50, 100, 200, 400, 800, 1200, 1200, 1200 ]


toHtml : Shell msg -> Html msg
toHtml (Shell config) =
    div [ class "ui-shell" ]
        [ -- Skip links come first in the DOM, which is the only place they work.
          a [ class "ui-skip-link", href "#ui-main" ] [ text config.words.skipToContent ]
        , a [ class "ui-skip-link", href "#ui-nav" ] [ text config.words.skipToNavigation ]
        , -- tabindex -1 so the skip link has somewhere to land: an element
          -- without it cannot take programmatic focus, and the jump silently
          -- does nothing.
          nav [ class "ui-shell__nav", id "ui-nav", tabindex -1 ] [ config.navigation ]
        , div [ class "ui-shell__body" ]
            [ case config.header of
                Just content ->
                    header [ class "ui-shell__header" ] [ content ]

                Nothing ->
                    text ""
            , main_
                [ class "ui-shell__main"
                , id "ui-main"

                -- -1 so it is focusable programmatically but not a tab stop.
                , tabindex -1
                ]
                config.content
            , case config.sidePanel of
                Just content ->
                    -- The panel is its own landmark (`Ui.Drawer` is an
                    -- `aside` named by its title), so the slot is not one.
                    div [ class "ui-shell__aside" ] [ content ]

                Nothing ->
                    text ""
            ]
        ]
