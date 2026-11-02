module Common.Components.PublicFrame exposing
    ( Brand
    , app
    , region
    , view
    )

{-| The frame around everything someone sees before signing in: the brand, the
few links a visitor has (log in, sign up, or back to the application), and the
page.

Two shapes, as `Ui.Page.Public` has. `view` is the panel with a title in it —
sign in, sign up, reset a password — optionally with an introduction beside it
(the login screen's information text). `app` is for a page with its own template
that a visitor may also open — a public knowledge model, a page that was not
found — where the page already has its `h1` and needs the width of a page.

The brand is not a link. Every public page's home is the login screen, so a
link on the logo would lead, from the login screen, to the login screen, and
from anywhere else to where the "Log In" link in the header already goes. It is
the application's name with the logo marked decorative: an `alt` repeating the
name beside it would have a screen reader read it twice.

The links are a named navigation landmark ("Public"), placed before the page in
the document because they are at the top of the screen. Banners and
dialogs go in `overlays`, after the page, where they are seen without
being the first thing reached.

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (alt, class, src)
import Ui.Page.Public as Public


type alias Brand =
    { title : String
    , logoUrl : Maybe String
    }


view :
    WithLocale a
    ->
        { title : String
        , brand : Brand
        , links : List { label : String, url : String }
        , content : List (Html msg)
        , intro : Maybe (Html msg)
        , aside : Maybe (Html msg)
        , footer : Maybe (Html msg)
        , overlays : List (Html msg)
        }
    -> Html msg
view appState config =
    let
        withOptional builder mbValue public =
            case mbValue of
                Just value ->
                    builder value public

                Nothing ->
                    public
    in
    div [ class "c-public-frame" ]
        (Public.public
            { title = config.title
            , brand = viewBrand config.brand
            , content = config.content
            }
            |> withLinks appState config.links
            |> withOptional Public.withIntro config.intro
            |> withOptional Public.withAside config.aside
            |> withOptional Public.withFooter config.footer
            |> Public.toHtml
            |> (\page -> page :: config.overlays)
        )


{-| A page that brings its own template and heading.
-}
app :
    WithLocale a
    ->
        { brand : Brand
        , links : List { label : String, url : String }
        , content : List (Html msg)
        , overlays : List (Html msg)
        }
    -> Html msg
app appState config =
    div [ class "c-public-frame" ]
        (Public.bare
            { brand = viewBrand config.brand
            , content = config.content
            }
            |> withLinks appState config.links
            |> Public.toHtml
            |> (\page -> page :: config.overlays)
        )


{-| An application page a visitor may open — a project shared by its link, a
public knowledge model — framed as the application frames it: the public header
across the top and the page filling the rest of the window, scrolling where it
scrolls when someone is signed in (`Ui.Page.Public.region`).
-}
region :
    WithLocale a
    ->
        { brand : Brand
        , links : List { label : String, url : String }
        , content : List (Html msg)
        , overlays : List (Html msg)
        }
    -> Html msg
region appState config =
    div [ class "c-public-frame" ]
        (Public.region
            { brand = viewBrand config.brand
            , content = config.content
            }
            |> withLinks appState config.links
            |> Public.toHtml
            |> (\page -> page :: config.overlays)
        )


withLinks : WithLocale a -> List { label : String, url : String } -> Public.Public msg -> Public.Public msg
withLinks appState links =
    Public.withLinks { label = gettext "Public" appState.locale, links = links }


viewBrand : Brand -> Html msg
viewBrand brand =
    div [ class "c-public-frame__brand" ]
        [ case brand.logoUrl of
            Just logoUrl ->
                img [ class "c-public-frame__logo", src logoUrl, alt "" ] []

            Nothing ->
                text ""
        , span [ class "c-public-frame__title" ] [ text brand.title ]
        ]
