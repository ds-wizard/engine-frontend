module Common.Pages.Public.Layout exposing (app, brand, links, panel)

{-| The frame of a public page — the brand and the links a visitor sees in the
header — the same on every public page of every client.
-}

import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Common.Components.PublicFrame as PublicFrame
import Common.Data.AppState exposing (AppState)
import Common.Data.Session as Session
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)


{-| A public page with a template and a heading of its own.
-}
app : AppState -> List (Html msg) -> Html msg
app appState content =
    PublicFrame.app appState
        { brand = brand appState
        , links = links appState Nothing
        , content = content
        , overlays = []
        }


{-| A public page with a panel: a title, and a form or a message in it, with an
optional introduction beside the panel and an aside below it (the login screen's
information and sidebar information). The brand
and the links are the ones `app` gives the bare frame, so a visitor sees
the same header on every public page.
-}
panel :
    AppState
    ->
        { title : String
        , content : List (Html msg)
        , intro : Maybe (Html msg)
        , aside : Maybe (Html msg)
        }
    -> Html msg
panel appState config =
    PublicFrame.view appState
        { title = config.title
        , brand = brand appState
        , links = links appState Nothing
        , content = config.content
        , intro = config.intro
        , aside = config.aside
        , footer = Nothing
        , overlays = []
        }


brand : AppState -> PublicFrame.Brand
brand appState =
    { title = LookAndFeelConfig.getAppTitle appState.config.lookAndFeel
    , logoUrl = Just (LookAndFeelConfig.getLogoUrl appState.config.lookAndFeel)
    }


{-| "Go to App" for someone with a session; "Log In" and, where registration
is open, "Sign Up" for everyone else.
-}
links : AppState -> Maybe String -> List { label : String, url : String }
links appState mbOriginalUrl =
    if Session.exists appState.session then
        [ { label = gettext "Go to App" appState.locale, url = Routes.toUrl appState Routes.appHome } ]

    else
        let
            signUp =
                if appState.config.authentication.registrationEnabled then
                    [ { label = gettext "Sign Up" appState.locale, url = Routes.toUrl appState Routes.publicSignup } ]

                else
                    []
        in
        { label = gettext "Log In" appState.locale
        , url = Routes.loginUrl mbOriginalUrl
        }
            :: signUp
