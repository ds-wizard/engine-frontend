module Common.Components.Page exposing
    ( actionResult
    , actionResultWithinPage
    , error
    , errorWithinPage
    , loading
    , misconfigured
    , notAllowed
    , notFound
    , notFoundEntity
    , notFoundEntityWithButton
    , placeholder
    )

{-| The states a whole page can be in other than its content: loading, failed,
not there, not yours, not configured, and not built yet.

Every one of them except `loading` is a `Ui.Page.Message`, which means every one
of them has the page's `h1`. That matters beyond looks: after a route change the
shell moves focus to the page's heading, and a page that failed to load still
has to have one for focus to land on and for the screen reader to announce
where the user is. An error is a page too.

The illustrations are decorative (`Common.Components.Illustration`); the
heading and the sentence under it say everything they do.

`loading` is `Ui.Page.Loading` — a status region with the words in it from the
start. It is for the region between a route and its data; a part of a page that
reloads while the rest stays put wants `Ui.AsyncBlock` instead.

**A tab of a page that loaded is not a page.** `actionResultWithinPage` and
`errorWithinPage` are the same states for the region under a detail page's tabs,
where the frame has already rendered the page's `h1`: the error's title is an
`h2` (`Ui.Page.Message.withinPage`), and its sentence is announced, because
nothing else tells a screen reader that the region changed. `loading` has no
heading and serves both.

-}

import ActionResult exposing (ActionResult(..))
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Button as Button
import Ui.Illustration as Illustration
import Ui.Page.Loading as Loading
import Ui.Page.Message as Message


loading : WithLocale a -> Html msg
loading appState =
    Loading.toHtml (gettext "Loading..." appState.locale)


{-| Nothing while nothing has been asked for, `loading` while waiting, `error`
with the message when it failed, and the content once it is there.
-}
actionResult : WithLocale a -> (x -> Html msg) -> ActionResult x -> Html msg
actionResult appState viewContent result =
    case result of
        Unset ->
            Html.text ""

        Loading ->
            loading appState

        Error message ->
            error appState message

        Success content ->
            viewContent content


{-| `actionResult` for a region of a page that already has its heading — a
detail tab loading its own data. Loading is the same full-height `loading`;
an error is `errorWithinPage`.
-}
actionResultWithinPage : WithLocale a -> (x -> Html msg) -> ActionResult x -> Html msg
actionResultWithinPage appState viewContent result =
    case result of
        Unset ->
            Html.text ""

        Loading ->
            loading appState

        Error message ->
            errorWithinPage appState message

        Success content ->
            viewContent content


{-| A page that could not be shown. The message is the specific one — "Unable
to get the project." — and the heading is the generic "Error", so the page
still has a heading that says what kind of page it is.
-}
error : WithLocale a -> String -> Html msg
error appState message =
    Message.message
        { title = gettext "Error" appState.locale
        , description = message
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> Message.toHtml


{-| `error` inside a page that already has its heading: the same illustration,
title and sentence, centered in the region, with the title as an `h2` so the page
keeps its one `h1`. The sentence is an alert — the error arrived while the user
was on the page, and focus is wherever they left it.
-}
errorWithinPage : WithLocale a -> String -> Html msg
errorWithinPage appState message =
    Message.message
        { title = gettext "Error" appState.locale
        , description = message
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> Message.withinPage
        |> Message.announced
        |> Message.toHtml


{-| The route matched nothing. The way back is a link, not a button: it goes
somewhere.
-}
notFound : WithLocale a -> { homeUrl : String } -> Html msg
notFound appState config =
    Message.message
        { title = gettext "Not Found" appState.locale
        , description = gettext "The page you are looking for was not found." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.notFound)
        |> Message.withAction (Button.linkPrimary (gettext "Go to dashboard" appState.locale) config.homeUrl)
        |> Message.toHtml


{-| The route is fine; the thing it names is not there — a project that was
deleted, an address someone mistyped or pasted from an older instance.

The sentence is the caller's, because only the caller knows what was being
looked for: "Not Found" over "Unable to get the project." is the difference
between a page that says what happened and one that says a request failed.

The way out is the caller's too. The list the thing would have been in is a step
towards finding it, where "Go to dashboard" only leaves.

-}
notFoundEntity : WithLocale a -> { message : String, back : { label : String, url : String } } -> Html msg
notFoundEntity appState config =
    Message.message
        { title = gettext "Not Found" appState.locale
        , description = config.message
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.notFound)
        |> Message.withAction (Button.linkPrimary config.back.label config.back.url)
        |> Message.toHtml


{-| `notFoundEntity` whose way out is a button rather than a link: somewhere
to choose from that opens over the page, such as a dialog with the other things
of the kind, where there is no page of them this user may see.
-}
notFoundEntityWithButton : WithLocale a -> { message : String, button : { label : String, onClick : msg } } -> Html msg
notFoundEntityWithButton appState config =
    Message.message
        { title = gettext "Not Found" appState.locale
        , description = config.message
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.notFound)
        |> Message.withAction (Button.primary config.button.label config.button.onClick)
        |> Message.toHtml


{-| The route exists and this user may not see it.

The way back is the caller's to offer: someone who is not signed in has no
dashboard, and a link there would only turn them away again.

-}
notAllowed : WithLocale a -> { homeUrl : Maybe String } -> Html msg
notAllowed appState config =
    let
        withHomeLink message =
            case config.homeUrl of
                Just homeUrl ->
                    Message.withAction (Button.linkPrimary (gettext "Go to dashboard" appState.locale) homeUrl) message

                Nothing ->
                    message
    in
    Message.message
        { title = gettext "Not Allowed" appState.locale
        , description = gettext "You don't have permission to view this page." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.notAllowed)
        |> withHomeLink
        |> Message.toHtml


{-| The application cannot start. There is no link back, because there is
nowhere in the application that would work.
-}
misconfigured : WithLocale a -> Html msg
misconfigured appState =
    Message.message
        { title = gettext "Configuration Error" appState.locale
        , description =
            gettext "Application is not configured correctly and cannot run." appState.locale
                ++ " "
                ++ gettext "Please, contact the administrator." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> Message.toHtml


{-| A page that exists in the route table but is not built on this interface
yet. It keeps the route's `h1`, so focus on a route change still lands and the
screen reader still says where the user is, and it offers the way back.

The words are the caller's, because only the caller knows what is missing. An
empty `backLabel` is said as "Go to dashboard": a link without a name is a link
nobody can choose.

-}
placeholder : WithLocale a -> { title : String, description : String, backUrl : String, backLabel : String } -> Html msg
placeholder appState config =
    let
        backLabel =
            if String.isEmpty (String.trim config.backLabel) then
                gettext "Go to dashboard" appState.locale

            else
                config.backLabel
    in
    Message.message
        { title = config.title
        , description = config.description
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.placeholder)
        |> Message.withAction (Button.link backLabel config.backUrl)
        |> Message.toHtml
