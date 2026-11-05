module Registry.Layout exposing
    ( Config
    , form
    , userMenuCount
    , userMenuId
    , view
    )

{-| The registry's one frame, the same whether someone is signed in or not: a
website about what is published rather than an application to work in.

A bar across the top has the brand, which leads home; the navigation — Home and
each kind of thing the registry publishes, the current one marked; and at its
end About, and the way in: Log in and Sign up, or for someone signed in a menu
with their name holding their account, its API keys, the accounts for an
administrator, and Log out. Under it the page fills the rest of the window, as
a page fills the application frame's region (`Ui.Page.Public.region` is the
same shape), so a detail page keeps its header while its body scrolls.

`form` is signing in, signing up and a forgotten password: the page's title
and its form in a panel in the middle of the same frame.

-}

import Common.Components.UserAvatar as UserAvatar
import Gettext exposing (gettext)
import Html exposing (Html, a, div, header, img, main_, section, span, text)
import Html.Attributes exposing (alt, class, href, id, src, tabindex)
import Registry.Api.Models.Item exposing (Kind)
import Registry.Api.Models.User as User
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Routes as Routes exposing (Route)
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Menu as Menu
import Ui.NavTabs as NavTabs
import Ui.Text as Text
import Ui.Tooltip as Tooltip


type alias Config msg =
    { route : Route
    , userMenu : Menu.Model
    , userMenuMsg : Menu.Msg -> msg
    , openAbout : msg
    , logout : msg
    }


view : AppState -> Config msg -> { content : List (Html msg), overlays : List (Html msg) } -> Html msg
view appState config page =
    div [ class "registry" ]
        (a [ class "ui-skip-link", href "#ui-main" ] [ text (gettext "Skip to content" appState.locale) ]
            :: header [ class "registry__header" ]
                [ a [ class "registry__brand", href (Routes.toUrl Routes.Home) ]
                    [ img [ class "registry__logo", src "/assets/logo.svg", alt "" ] []
                    , span [ class "registry__title" ] [ text appState.appTitle ]
                    ]
                , viewNavigation appState config.route
                , div [ class "registry__actions" ] (viewActions appState config)
                ]
            :: main_ [ class "registry__main", id "ui-main", tabindex -1 ] page.content
            :: page.overlays
        )


form : AppState -> Config msg -> { title : String, content : List (Html msg) } -> Html msg
form appState config page =
    view appState
        config
        { content =
            [ div [ class "registry__form" ]
                [ section [ class "registry__panel" ] (Text.pageTitle page.title :: page.content) ]
            ]
        , overlays = []
        }



-- NAVIGATION


viewNavigation : AppState -> Route -> Html msg
viewNavigation appState route =
    let
        tab { icon, label, url, active, testId } =
            NavTabs.item { label = label, url = url, active = active }
                |> NavTabs.withIcon icon
                |> NavTabs.withTestId ("nav_" ++ testId)

        homeTab =
            tab
                { icon = Icon.solid "fa-house"
                , label = gettext "Home" appState.locale
                , url = Routes.toUrl Routes.Home
                , active = route == Routes.Home
                , testId = "home"
                }

        sectionTab section =
            tab
                { icon = section.icon
                , label = section.title
                , url = Routes.toUrl (Routes.Items section.kind)
                , active = List.any (\kind -> isKind kind route) section.kinds
                , testId = section.testId
                }
    in
    div [ class "registry__nav" ]
        [ NavTabs.toHtml
            { label = gettext "Main" appState.locale
            , items = homeTab :: List.map sectionTab (Kind.sections appState)
            , full = False
            }
        ]


isKind : Kind -> Route -> Bool
isKind kind route =
    case route of
        Routes.Items k ->
            k == kind

        Routes.ItemDetail k _ ->
            k == kind

        _ ->
            False



-- ABOUT AND THE ACCOUNT


viewActions : AppState -> Config msg -> List (Html msg)
viewActions appState config =
    let
        about =
            Button.iconOnly (Icon.solid "fa-circle-info") (gettext "About" appState.locale) config.openAbout
                |> Button.quiet
                |> Button.withTooltipAt Tooltip.Below
                |> Button.toHtml
    in
    case appState.user of
        Just user ->
            [ about
            , Menu.toHtml
                { id = userMenuId
                , trigger =
                    Menu.custom
                        { label = User.fullName user
                        , class = "ui-button ui-button--quiet"
                        , content =
                            [ ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (UserAvatar.item (User.fullName user) Nothing))
                            , text (User.fullName user)
                            , Icon.toHtml (Icon.solid "fa-chevron-down")
                            ]
                        }
                        |> Menu.withTriggerTestId "user-menu"
                , alignment = Menu.End
                , items = userMenuItems appState config
                , toMsg = config.userMenuMsg
                }
                config.userMenu
            ]

        Nothing ->
            about
                :: Button.toHtml (Button.linkGhost (gettext "Log in" appState.locale) (Routes.toUrl (Routes.Login Nothing)))
                :: (if appState.config.publicRegistrationEnabled then
                        [ Button.toHtml (Button.link (gettext "Sign up" appState.locale) (Routes.toUrl Routes.Signup)) ]

                    else
                        []
                   )


userMenuItems : AppState -> Config msg -> List (Menu.Item msg)
userMenuItems appState config =
    [ Menu.withIcon (Icon.solid "fa-user-pen") (Menu.linkItem (gettext "Account" appState.locale) (Routes.toUrl Routes.Account))
    , Menu.withIcon (Icon.solid "fa-key") (Menu.linkItem (gettext "API Keys" appState.locale) (Routes.toUrl Routes.ApiKeys))
    ]
        ++ (if AppState.isAdmin appState then
                [ Menu.withIcon (Icon.solid "fa-users") (Menu.linkItem (gettext "Users" appState.locale) (Routes.toUrl Routes.Users)) ]

            else
                []
           )
        ++ [ Menu.separator
           , Menu.withIcon (Icon.solid "fa-right-from-bracket") (Menu.item (gettext "Log out" appState.locale) config.logout)
                |> Menu.withTestId "user-menu_logout"
           ]


userMenuId : String
userMenuId =
    "registry-user-menu"


{-| How many items the account menu has, which `Ui.Menu.update` moves focus
among.
-}
userMenuCount : AppState -> Int
userMenuCount appState =
    if AppState.isAdmin appState then
        4

    else
        3
