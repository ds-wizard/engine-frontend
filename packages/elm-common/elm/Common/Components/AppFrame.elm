module Common.Components.AppFrame exposing
    ( Brand
    , Context
    , MenuEntry
    , Model
    , Msg
    , ViewConfig
    , child
    , closeContextMenu
    , closeUserMenu
    , destinations
    , group
    , heading
    , init
    , isCollapsed
    , item
    , sectionBreak
    , subscriptions
    , update
    , view
    , withBadge
    , withId
    , withNewTab
    , withTestId
    )

{-| The authenticated application frame: navigation, account, and the page.

Built from `Ui.AppShell`, `Ui.SideNav` and `Ui.Menu`, so the skip links, the
landmarks, the one-tab-stop navigation, the collapsing rail and the resizable
edge all come from there. What this adds is the part every application fills in
the same way: the brand, the entries, the account
menu with the appearance switch in it, and a place for a notice at the foot of
the navigation.

It knows nothing about any one application. The entries arrive already filtered
by permission and already marked active; the account menu's items arrive built;
the appearance is the application's to store and apply. That is what lets a
second application use it unchanged.

**State.** The navigation collapses and resizes, and the account menu opens and closes,
so there is a `Model`. Persisting it is the application's: `update` calls back
with the collapsed flag and the width, but only once they have settled — a drag
reports nothing until the pointer is released, because writing every frame of a
drag to storage writes sixty values to learn one.

**Focus.** Choosing an item that opens a dialog should send `closeUserMenu`
(or `closeContextMenu`, from the context's menu) first. Closing puts focus back on the account button, which is then what the
dialog gives focus back to when it closes; a dialog opened straight from an item
returns focus to an item that no longer exists, and focus falls to the document.

**Overlays** — dialogs, banners — are rendered after the shell in
document order. They are seen when they are open, and never reached before the
page is.

-}

import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (alt, attribute, class, href, src)
import Ui.AppShell as AppShell
import Ui.Icon as Icon exposing (Icon)
import Ui.ItemIcon as ItemIcon exposing (ItemIcon)
import Ui.Menu as Menu
import Ui.SideNav as SideNav
import Ui.Theme as Theme



-- MODEL


type Model
    = Model
        { sideNav : SideNav.Model
        , userMenu : Menu.Model
        , contextMenu : Menu.Model
        , persisted : { collapsed : Bool, width : Float }
        }


{-| The stored preference, as the application kept it. A missing width is the
navigation's own default.
-}
init : { collapsed : Bool, width : Maybe Float } -> Model
init stored =
    let
        sideNav =
            SideNav.restore stored
    in
    Model
        { sideNav = sideNav
        , userMenu = Menu.init
        , contextMenu = Menu.init
        , persisted = { collapsed = SideNav.isCollapsed sideNav, width = SideNav.widthPx sideNav }
        }


isCollapsed : Model -> Bool
isCollapsed (Model model) =
    SideNav.isCollapsed model.sideNav


userMenuId : String
userMenuId =
    "c-user-menu"


contextMenuId : String
contextMenuId =
    "c-context-menu"



-- UPDATE


{-| The account menu needs to know how many stops it has to wrap the arrow keys
around, and only the view knows what is in it — so the count travels with the
message rather than being guessed here.
-}
type Msg
    = SideNavMsg SideNav.Msg
    | UserMenuMsg Int Menu.Msg
    | ContextMenuMsg Int Menu.Msg


{-| Close the account menu and return focus to its button. See the module
documentation for when.
-}
closeUserMenu : Msg
closeUserMenu =
    UserMenuMsg 0 Menu.close


{-| Close the context's menu — the place above the navigation — and return focus
to its control, for an item of it that opens a dialog. See the module
documentation.
-}
closeContextMenu : Msg
closeContextMenu =
    ContextMenuMsg 0 Menu.close


update :
    { storeCollapsed : Bool -> Cmd msg
    , storeWidth : Float -> Cmd msg
    , toMsg : Msg -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update cfg msg (Model model) =
    case msg of
        SideNavMsg subMsg ->
            let
                sideNav =
                    SideNav.update subMsg model.sideNav
            in
            if SideNav.isDragging sideNav then
                ( Model { model | sideNav = sideNav }, Cmd.none )

            else
                let
                    current =
                        { collapsed = SideNav.isCollapsed sideNav, width = SideNav.widthPx sideNav }
                in
                ( Model { model | sideNav = sideNav, persisted = current }
                , Cmd.batch
                    [ if current.collapsed /= model.persisted.collapsed then
                        cfg.storeCollapsed current.collapsed

                      else
                        Cmd.none
                    , if current.width /= model.persisted.width then
                        cfg.storeWidth current.width

                      else
                        Cmd.none
                    ]
                )

        UserMenuMsg count subMsg ->
            let
                ( userMenu, cmd ) =
                    Menu.update userMenuId count subMsg model.userMenu
            in
            ( Model { model | userMenu = userMenu }
            , Cmd.map (cfg.toMsg << UserMenuMsg count) cmd
            )

        ContextMenuMsg count subMsg ->
            let
                ( contextMenu, cmd ) =
                    Menu.update contextMenuId count subMsg model.contextMenu
            in
            ( Model { model | contextMenu = contextMenu }
            , Cmd.map (cfg.toMsg << ContextMenuMsg count) cmd
            )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Sub.map SideNavMsg (SideNav.subscriptions model.sideNav)
        , Sub.map (UserMenuMsg 0) (Menu.subscriptions userMenuId model.userMenu)
        , Sub.map (ContextMenuMsg 0) (Menu.subscriptions contextMenuId model.contextMenu)
        ]



-- ENTRIES


{-| An entry in the navigation. Top-level entries have an icon, because the rail
shows nothing else; entries inside a group do not.
-}
type MenuEntry msg
    = MenuEntry
        { icon : Maybe Icon
        , label : String
        , url : String
        , active : Bool
        , badge : Maybe (Html msg)
        , id : Maybe String
        , testId : Maybe String
        , newTab : Bool
        , children : List (MenuEntry msg)
        }
    | Heading String
    | SectionBreak


{-| A top-level link.
-}
item : Icon -> { label : String, url : String, active : Bool } -> MenuEntry msg
item icon config =
    MenuEntry
        { icon = Just icon
        , label = config.label
        , url = config.url
        , active = config.active
        , badge = Nothing
        , id = Nothing
        , testId = Nothing
        , newTab = False
        , children = []
        }


{-| A top-level entry with links under it. Its own `url` is where the group
itself goes — usually its first child — so the group is a link too, and in the
rail it is the only thing the icon can be.
-}
group : Icon -> { label : String, url : String, active : Bool } -> List (MenuEntry msg) -> MenuEntry msg
group icon config children =
    case item icon config of
        MenuEntry entry ->
            MenuEntry { entry | children = children }

        entry ->
            entry


{-| The menu as a flat list of pages, for something that offers them somewhere
other than the navigation — the command palette.

Only the pages. A group's own link goes wherever its first child goes, so it is
left out: two rows that open the same page, one of them named after a section
rather than after what it opens, is what a list you type into must not have. A
top-level entry with no children is a page and is kept, under its own name.

Headings are not pages either. What is here is what the navigation shows, so an
entry hidden by a permission is missing from both.

-}
destinations :
    List (MenuEntry msg)
    -> List { id : String, label : String, section : String, url : String, leading : Maybe (Html Never) }
destinations entries =
    let
        flatten section entry =
            case entry of
                MenuEntry config ->
                    let
                        own =
                            -- A link that leaves the application is not one of
                            -- its pages.
                            if List.isEmpty config.children && not config.newTab then
                                [ { id = Maybe.withDefault config.url config.id
                                  , label = config.label
                                  , section = Maybe.withDefault config.label section
                                  , url = config.url
                                  , leading = Nothing
                                  }
                                ]

                            else
                                []
                    in
                    own ++ List.concatMap (flatten (Just config.label)) config.children

                Heading _ ->
                    []

                SectionBreak ->
                    []
    in
    List.concatMap (flatten Nothing) entries


{-| A link inside a group.
-}
child : { label : String, url : String, active : Bool } -> MenuEntry msg
child config =
    MenuEntry
        { icon = Nothing
        , label = config.label
        , url = config.url
        , active = config.active
        , badge = Nothing
        , id = Nothing
        , testId = Nothing
        , newTab = False
        , children = []
        }


withBadge : Html msg -> MenuEntry msg -> MenuEntry msg
withBadge badge menuEntry =
    case menuEntry of
        MenuEntry entry ->
            MenuEntry { entry | badge = Just badge }

        _ ->
            menuEntry


{-| An `id` on the entry's link, so something outside the navigation can find
it — a guided tour pointing at "Projects", say (`Ui.SideNav.withId`). Works on
`item`, `group` and `child` alike; it has to be unique on the page.
-}
withId : String -> MenuEntry msg -> MenuEntry msg
withId entryId menuEntry =
    case menuEntry of
        MenuEntry entry ->
            MenuEntry { entry | id = Just entryId }

        _ ->
            menuEntry


{-| A `data-cy` test hook on the entry's link (`Ui.SideNav.withTestId`), apart
from `withId`. Works on `item`, `group` and `child` alike.
-}
withTestId : String -> MenuEntry msg -> MenuEntry msg
withTestId testId menuEntry =
    case menuEntry of
        MenuEntry entry ->
            MenuEntry { entry | testId = Just testId }

        _ ->
            menuEntry


{-| Open the entry's link in a new tab, and say so (`Ui.SideNav.withNewTab`):
for a link that leaves the application, like a guide.
-}
withNewTab : MenuEntry msg -> MenuEntry msg
withNewTab menuEntry =
    case menuEntry of
        MenuEntry entry ->
            MenuEntry { entry | newTab = True }

        _ ->
            menuEntry


{-| A section's worth of air between the entries before it and those after:
what follows is next to the navigation rather than part of it — a way out to
somewhere else, a guide. Only meaningful at the top level.
-}
sectionBreak : MenuEntry msg
sectionBreak =
    SectionBreak


{-| A heading over the top-level entries that follow it, up to the next heading.
The entries after it are a group named by it (`Ui.SideNav.heading`); in the rail
it is hidden visually and still names the group. Only meaningful at the top
level.
-}
heading : String -> MenuEntry msg
heading =
    Heading


toSideNavItem : MenuEntry msg -> SideNav.Item msg
toSideNavItem menuEntry =
    case menuEntry of
        MenuEntry entry ->
            toSideNavEntry entry

        Heading label ->
            SideNav.heading label

        SectionBreak ->
            SideNav.sectionBreak


toSideNavEntry :
    { icon : Maybe Icon
    , label : String
    , url : String
    , active : Bool
    , badge : Maybe (Html msg)
    , id : Maybe String
    , testId : Maybe String
    , newTab : Bool
    , children : List (MenuEntry msg)
    }
    -> SideNav.Item msg
toSideNavEntry entry =
    let
        base =
            case entry.icon of
                Just icon ->
                    SideNav.group icon { label = entry.label, url = entry.url, active = entry.active }

                Nothing ->
                    SideNav.item { label = entry.label, url = entry.url, active = entry.active }

        withChildren sideNavItem =
            if List.isEmpty entry.children then
                sideNavItem

            else
                SideNav.withChildren (List.map toSideNavItem entry.children) sideNavItem

        withEntryId sideNavItem =
            case entry.id of
                Just entryId ->
                    SideNav.withId entryId sideNavItem

                Nothing ->
                    sideNavItem

        withEntryTestId sideNavItem =
            case entry.testId of
                Just testId ->
                    SideNav.withTestId testId sideNavItem

                Nothing ->
                    sideNavItem

        withEntryBadge sideNavItem =
            case entry.badge of
                Just badge ->
                    SideNav.withBadge badge sideNavItem

                Nothing ->
                    sideNavItem
    in
    base
        |> withEntryBadge
        |> withEntryId
        |> withEntryTestId
        |> (if entry.newTab then
                SideNav.withNewTab

            else
                identity
           )
        |> withChildren



-- VIEW


type alias Brand =
    { title : String
    , logoUrl : Maybe String
    , homeUrl : String
    }


{-| What the navigation is the navigation of, where an application has more
than one such place, and the menu of the others (`Ui.SideNav.context`): the
place's name and icon, the control's accessible name, the menu's items, and
the control's test hook (`data-cy`), if it has one.
-}
type alias Context msg =
    { name : String
    , label : String
    , icon : ItemIcon
    , items : List (Menu.Item msg)
    , testId : Maybe String
    }


{-| What the frame shows.

  - `context` — where the navigation belongs, between the brand and the
    entries, for an application that has several such places.
  - `items` — the navigation, filtered and marked active by the application.
  - `customLinks` — the instance's own links, a section of their own after the
    application's entries.
    `icon` is a Font Awesome class string, weight included or not
    (`Ui.Icon.fromClass`); `newWindow` opens the link in a new
    tab and says so.
  - `user` and `userMenu` — the account. The appearance switch is placed between
    `userMenu.items` and `userMenu.moreItems`.
  - `footerNotice` — something that has to stay in sight, like a session about to
    end. It sits above the account at the foot of the navigation.
  - `footerActions` — controls at the foot of the navigation, above the account,
    that change the frame rather than the page: `Ui.SideNav.panelToggle`s.
    Unlike the notice they stay in the rail, as icons.
  - `sidePanel` — the panel one of them opens (`Ui.Drawer`), while it is open;
    `Nothing` leaves the frame as it is without one.

-}
type alias ViewConfig msg =
    { brand : Brand
    , context : Maybe (Context msg)
    , items : List (MenuEntry msg)
    , customLinks : List { label : String, url : String, icon : String, newWindow : Bool }
    , user : { name : String, role : String, avatar : ItemIcon }
    , userMenu : { items : List (Menu.Item msg), moreItems : List (Menu.Item msg) }
    , mode : Theme.Mode
    , setMode : Theme.Mode -> msg
    , footerNotice : Maybe (Html msg)
    , footerActions : List (Html msg)
    , sidePanel : Maybe (Html msg)
    , toMsg : Msg -> msg
    , content : List (Html msg)
    , overlays : List (Html msg)
    }


view : WithLocale a -> ViewConfig msg -> Model -> Html msg
view appState config (Model model) =
    let
        customItems =
            if List.isEmpty config.customLinks then
                []

            else
                SideNav.sectionBreak :: List.map customItem config.customLinks

        customItem link =
            SideNav.group (Icon.fromClass link.icon)
                { label = gettext link.label appState.locale
                , url = link.url
                , active = False
                }
                -- The link's address is what identifies it: the instance sets it.
                |> SideNav.withTestId ("menu_custom-link_" ++ link.url)
                |> (if link.newWindow then
                        SideNav.withNewTab

                    else
                        identity
                   )

        navigation =
            SideNav.toHtmlWithWords (UiWords.sideNav appState)
                { label = gettext "Main" appState.locale
                , model = model.sideNav
                , toMsg = config.toMsg << SideNavMsg
                , brand = viewBrand config True
                , brandCompact = viewBrand config False
                , context = Maybe.map (viewContext config model) config.context
                , items = List.map toSideNavItem config.items ++ customItems
                , footer =
                    Just
                        (div [ class "c-app-frame__footer" ]
                            [ case config.footerNotice of
                                Just notice ->
                                    div [ class "c-app-frame__notice" ] [ notice ]

                                Nothing ->
                                    text ""
                            , if List.isEmpty config.footerActions then
                                text ""

                              else
                                div [ class "c-app-frame__actions" ] config.footerActions
                            , viewUser appState config model
                            ]
                        )
                }
    in
    div [ class "c-app-frame" ]
        (AppShell.toHtml
            (AppShell.shell { navigation = navigation, content = config.content }
                |> AppShell.withWords (UiWords.appShell appState)
                |> (case config.sidePanel of
                        Just panel ->
                            AppShell.withSidePanel panel

                        Nothing ->
                            identity
                   )
            )
            :: config.overlays
        )


viewBrand : ViewConfig msg -> Bool -> Html msg
viewBrand config full =
    let
        logo =
            case config.brand.logoUrl of
                Just logoUrl ->
                    -- Decorative: the title is the name, beside it or in the
                    -- link's label.
                    img [ class "c-app-frame__logo", src logoUrl, alt "" ] []

                Nothing ->
                    ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (ItemIcon.view config.brand.title))
    in
    if full then
        a [ href config.brand.homeUrl ] [ logo, span [ class "c-app-frame__title" ] [ text config.brand.title ] ]

    else
        -- The rail has room for the logo only, so the title becomes the link's
        -- name rather than disappearing from it.
        a [ href config.brand.homeUrl, attribute "aria-label" config.brand.title ] [ logo ]


viewContext : ViewConfig msg -> { m | contextMenu : Menu.Model } -> Context msg -> Html msg
viewContext config model context =
    SideNav.context
        { id = contextMenuId
        , label = context.label
        , name = context.name
        , icon = ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small context.icon)
        , items = context.items
        , menu = model.contextMenu
        , toMsg = config.toMsg << ContextMenuMsg (Menu.focusableCount context.items)
        , testId = context.testId
        }


viewUser : WithLocale a -> ViewConfig msg -> { m | userMenu : Menu.Model } -> Html msg
viewUser appState config model =
    let
        modeOption value label icon =
            { label = label
            , icon = Just (Icon.solid icon)
            , selected = config.mode == value
            , onSelect = config.setMode value
            }

        items =
            config.userMenu.items
                ++ [ Menu.separator
                   , Menu.segmented
                        { label = gettext "Appearance" appState.locale
                        , options =
                            [ modeOption Theme.Light (gettext "Light" appState.locale) "fa-sun"
                            , modeOption Theme.Dark (gettext "Dark" appState.locale) "fa-moon"
                            , modeOption Theme.System (gettext "System" appState.locale) "fa-desktop"
                            ]
                        }
                   , Menu.separator
                   ]
                ++ config.userMenu.moreItems
    in
    SideNav.user
        { id = userMenuId
        , name = config.user.name
        , role = config.user.role
        , avatar = ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Medium config.user.avatar)
        , avatarCompact = ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small config.user.avatar)
        , items = items
        , menu = model.userMenu
        , toMsg = config.toMsg << UserMenuMsg (Menu.focusableCount items)
        }
