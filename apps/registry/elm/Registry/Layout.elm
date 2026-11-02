module Registry.Layout exposing
    ( FrameConfig
    , browse
    , form
    , signedIn
    )

{-| The registry's frames.

  - **`browse`** — what a visitor sees of what is published: the public header
    with the catalogs and the way to sign in, and the page under it
    (`PublicFrame.region`).
  - **`form`** — signing in, signing up, a forgotten password: the public panel
    with the page's title in it (`PublicFrame.view`).
  - **`signedIn`** — everything for someone with an account: the application
    frame with the catalogs, and for an administrator the accounts, in the
    navigation, and the account and its API keys in the account menu.

-}

import Common.Components.AppFrame as AppFrame
import Common.Components.PublicFrame as PublicFrame
import Common.Components.UserAvatar as UserAvatar
import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Api.Models.Item exposing (Kind)
import Registry.Api.Models.User as User
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Routes as Routes exposing (Route)
import Ui.Icon as Icon
import Ui.Menu as Menu
import Ui.Theme as Theme


brand : AppState -> PublicFrame.Brand
brand appState =
    { title = title appState, logoUrl = Just "/assets/logo.svg" }


title : AppState -> String
title appState =
    gettext "Registry" appState.locale


publicLinks : AppState -> List { label : String, url : String }
publicLinks appState =
    List.map (\kind -> { label = Kind.title appState kind, url = Routes.toUrl (Routes.Items kind) }) (Kind.all appState)
        ++ { label = gettext "Log in" appState.locale, url = Routes.toUrl (Routes.Login Nothing) }
        :: (if appState.config.publicRegistrationEnabled then
                [ { label = gettext "Sign up" appState.locale, url = Routes.toUrl Routes.Signup } ]

            else
                []
           )


browse : AppState -> List (Html msg) -> Html msg
browse appState content =
    PublicFrame.region appState
        { brand = brand appState
        , links = publicLinks appState
        , content = content
        , overlays = []
        }


form : AppState -> { title : String, content : List (Html msg) } -> Html msg
form appState page =
    PublicFrame.view appState
        { title = page.title
        , brand = brand appState
        , links = publicLinks appState
        , content = page.content
        , intro = Nothing
        , aside = Nothing
        , footer = Nothing
        , overlays = []
        }


type alias FrameConfig msg =
    { frame : AppFrame.Model
    , frameMsg : AppFrame.Msg -> msg
    , setMode : Theme.Mode -> msg
    , logoutMsg : msg
    , route : Route
    }


signedIn : AppState -> FrameConfig msg -> List (Html msg) -> Html msg
signedIn appState config content =
    let
        ( name, role ) =
            case appState.user of
                Just user ->
                    ( User.fullName user
                    , if User.isAdmin user then
                        gettext "Admin" appState.locale

                      else
                        gettext "User" appState.locale
                    )

                Nothing ->
                    ( "", "" )

        kindEntry kind =
            AppFrame.item (Kind.icon kind)
                { label = Kind.title appState kind
                , url = Routes.toUrl (Routes.Items kind)
                , active = isKind kind config.route
                }

        usersEntry =
            if AppState.isAdmin appState then
                [ AppFrame.item (Icon.solid "fa-users") { label = gettext "Users" appState.locale, url = Routes.toUrl Routes.Users, active = config.route == Routes.Users } ]

            else
                []
    in
    AppFrame.view appState
        { brand = { title = title appState, logoUrl = Just "/assets/logo.svg", homeUrl = Routes.toUrl Routes.Home }
        , context = Nothing
        , items =
            AppFrame.item (Icon.solid "fa-house") { label = gettext "Home" appState.locale, url = Routes.toUrl Routes.Home, active = config.route == Routes.Home }
                :: List.map kindEntry (Kind.all appState)
                ++ usersEntry
        , customLinks = []
        , user = { name = name, role = role, avatar = UserAvatar.item name Nothing }
        , userMenu =
            { items =
                [ Menu.withIcon (Icon.solid "fa-user-pen") (Menu.linkItem (gettext "Account" appState.locale) (Routes.toUrl Routes.Account))
                , Menu.withIcon (Icon.solid "fa-key") (Menu.linkItem (gettext "API Keys" appState.locale) (Routes.toUrl Routes.ApiKeys))
                ]
            , moreItems = [ Menu.withIcon (Icon.solid "fa-right-from-bracket") (Menu.item (gettext "Log out" appState.locale) config.logoutMsg) ]
            }
        , mode = appState.uiMode
        , setMode = config.setMode
        , footerNotice = Nothing
        , footerActions = []
        , sidePanel = Nothing
        , toMsg = config.frameMsg
        , content = content
        , overlays = []
        }
        config.frame


isKind : Kind -> Route -> Bool
isKind kind route =
    case route of
        Routes.Items k ->
            k == kind

        Routes.ItemDetail k _ ->
            k == kind

        _ ->
            False
