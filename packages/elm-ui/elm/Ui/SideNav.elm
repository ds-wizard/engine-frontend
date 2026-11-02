module Ui.SideNav exposing
    ( Item
    , Model
    , Msg
    , Words
    , context
    , group
    , heading
    , init
    , isCollapsed
    , isDragging
    , item
    , panelToggle
    , restore
    , sectionBreak
    , setCollapsed
    , subscriptions
    , toHtml
    , toHtmlWithWords
    , update
    , user
    , widthPx
    , withBadge
    , withChildren
    , withExternalNotice
    , withId
    , withNewTab
    , withTestId
    )

{-| The application's primary navigation.

One tab stop for the links. Tab reaches the navigation, then the arrow keys move
through it — a sidebar with twenty links should not be twenty stops on the way to
the page content, which is what the skip link exists to avoid and what a roving
tabindex avoids more gracefully.

The links are presentational: the caller says which item is active. Two things
are not, and they are why this module carries state:

  - **Collapsing** to an icon rail, which the existing sidebar has and which
    people use. The label stays in the DOM when collapsed — hidden visually, so
    each link keeps its accessible name — and a tooltip beside the icon says the
    same thing to everyone else.
  - **Resizing** by dragging the trailing edge, which is `Ui.Resizer`. A menu
    holding project names has no width that is right for everyone, and picking
    one for them is how you end up with columns of truncated text.

Both are the app's to persist. The wizard keeps the collapsed flag in the
session; whatever the app already has is a better home than a port here.

-}

import Html exposing (Html, button, div, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, rel, style, target, type_)
import Html.Events
import Maybe.Extra as Maybe
import Ui.Icon as Icon exposing (Icon)
import Ui.Menu as Menu
import Ui.Resizer as Resizer
import Ui.TestId as TestId
import Ui.Tooltip as Tooltip



-- STATE


type Model
    = Model { resizer : Resizer.Model }


{-| The rail is 3.5rem in `_side-nav.scss`; the number here is the same size in
pixels, and it is only ever read to give the separator a starting point when a
collapsed sidebar is dragged open.

The bounds are wide enough to hold a project name and narrow enough that the
content still has a page: below the lower one, dragging collapses instead.

-}
init : Model
init =
    Model
        { resizer =
            Resizer.init
                { edge = Resizer.Trailing
                , label = "Resize navigation"
                , min = 180
                , max = 420
                , default = 240
                , collapse = Just { at = 132, rail = 56 }
                }
        }


{-| Rebuild the state the app stored.
-}
restore : { collapsed : Bool, width : Maybe Float } -> Model
restore stored =
    let
        (Model model) =
            init
    in
    Model { model | resizer = Resizer.restore stored model.resizer }


isCollapsed : Model -> Bool
isCollapsed (Model model) =
    Resizer.isCollapsed model.resizer


widthPx : Model -> Float
widthPx (Model model) =
    Resizer.widthPx model.resizer


{-| True while the pointer is down on the separator.

For the app's storage: persisting on every pointer move would write to
`localStorage` sixty times a second for the whole of a drag, and the only value
worth keeping is the one it ends on.

-}
isDragging : Model -> Bool
isDragging (Model model) =
    Resizer.isDragging model.resizer



-- UPDATE


type Msg
    = ResizerMsg Resizer.Msg


{-| Collapse or expand from outside — below a breakpoint, say. Everything else
the navigation does to itself.
-}
setCollapsed : Bool -> Msg
setCollapsed =
    ResizerMsg << Resizer.setCollapsed


update : Msg -> Model -> Model
update (ResizerMsg subMsg) (Model model) =
    Model { model | resizer = Resizer.update subMsg model.resizer }


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.map ResizerMsg (Resizer.subscriptions model.resizer)



-- ITEMS


type Item msg
    = Item
        { label : String
        , url : String
        , icon : Maybe Icon
        , active : Bool
        , badge : Maybe (Html msg)
        , children : List (Item msg)
        , newTab : Bool
        , externalNotice : Maybe String
        , id : Maybe String
        , testId : Maybe String
        }
    | Heading String
    | SectionBreak


item : { label : String, url : String, active : Bool } -> Item msg
item config =
    Item
        { label = config.label
        , url = config.url
        , icon = Nothing
        , active = config.active
        , badge = Nothing
        , children = []
        , newTab = False
        , externalNotice = Nothing
        , id = Nothing
        , testId = Nothing
        }


{-| A section. `active` means the page is somewhere in it: the group opens and
takes the section's tint, and the page itself is picked out only when one of its
children is `active`. A page in the section that no child names — a detail page
under its list — leaves every row in the group plain.
-}
group : Icon -> { label : String, url : String, active : Bool } -> Item msg
group icon config =
    mapEntry (\entry -> { entry | icon = Just icon }) (item config)


withChildren : List (Item msg) -> Item msg -> Item msg
withChildren children =
    mapEntry (\entry -> { entry | children = children })


withBadge : Html msg -> Item msg -> Item msg
withBadge badge =
    mapEntry (\entry -> { entry | badge = Just badge })


{-| Opens in a new tab — a link to documentation, or to another application —
and says so: `target="_blank"` with `rel="noopener noreferrer"`, an icon for the
eye, and "(opens in a new tab)" at the end of the link's accessible name, which
`withExternalNotice` or `toHtmlWithWords` puts in the language of the page
(WCAG 3.2.5).
-}
withNewTab : Item msg -> Item msg
withNewTab =
    mapEntry (\entry -> { entry | newTab = True })


{-| The words `withNewTab` adds, for this item only. Without it they are the
navigation's — `Words.newTab`.
-}
withExternalNotice : String -> Item msg -> Item msg
withExternalNotice notice =
    mapEntry (\entry -> { entry | externalNotice = Just notice })


{-| An `id` on the item's link, so something outside the navigation can find it —
a guided tour pointing at "Projects", say. It has to be unique on the page, and
a group's children are rendered once whether the rail is showing or not, so one
id is one element.
-}
withId : String -> Item msg -> Item msg
withId itemId =
    mapEntry (\entry -> { entry | id = Just itemId })


{-| A `data-cy` attribute on the item's link, the test hook. Apart from
`withId`, which other elements refer to, so either can change on its own.
-}
withTestId : String -> Item msg -> Item msg
withTestId testId =
    mapEntry (\entry -> { entry | testId = Just testId })


{-| A heading over the items that follow it, up to the next heading.

Not a link and not a stop: the items after it become a group named by it
(`role="group"` with `aria-labelledby`), so a screen reader entering the group
hears the heading once rather than stumbling over a line of text between links.
In the rail it is hidden visually and keeps naming its group.

-}
heading : String -> Item msg
heading =
    Heading


{-| Ends the section of items before it, with no heading for the next: the items
after it are a list of their own, with the space between sections above them.
For entries that belong to the navigation but not with the ones before them — an
instance's own links after the application's.
-}
sectionBreak : Item msg
sectionBreak =
    SectionBreak


mapEntry : (Entry msg -> Entry msg) -> Item msg -> Item msg
mapEntry fn candidate =
    case candidate of
        Item entry ->
            Item (fn entry)

        _ ->
            candidate


type alias Entry msg =
    { label : String
    , url : String
    , icon : Maybe Icon
    , active : Bool
    , badge : Maybe (Html msg)
    , children : List (Item msg)
    , newTab : Bool
    , externalNotice : Maybe String
    , id : Maybe String
    , testId : Maybe String
    }


{-| Whether this item, or anything under it, is the current page.
-}
isActive : Item msg -> Bool
isActive candidate =
    case candidate of
        Item entry ->
            entry.active || List.any isActive entry.children

        _ ->
            False



-- VIEW


{-| `brandCompact` is not optional, because the rail is not an afterthought: 56
pixels is not a small logo, it is a different one, and the caller is the only
one who knows what theirs reduces to.
-}
toHtml :
    { label : String
    , brand : Html msg
    , brandCompact : Html msg
    , context : Maybe (Html msg)
    , items : List (Item msg)
    , footer : Maybe (Html msg)
    , model : Model
    , toMsg : Msg -> msg
    }
    -> Html msg
toHtml =
    toHtmlWithWords english


{-| What the navigation says of its own accord: the collapse toggle in both
states, the name of the resize separator and its value — collapsed, or a width
in pixels — and the notice after a link that opens in a new tab. `toHtml` says
them in English.
-}
type alias Words =
    { collapse : String
    , expand : String
    , resize : String
    , collapsed : String
    , width : Int -> String
    , newTab : String
    }


english : Words
english =
    { collapse = "Collapse sidebar"
    , expand = "Expand sidebar"
    , resize = "Resize navigation"
    , collapsed = "Collapsed"
    , width = \pixels -> String.fromInt pixels ++ " pixels"
    , newTab = "(opens in a new tab)"
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords :
    Words
    ->
        { label : String
        , brand : Html msg
        , brandCompact : Html msg
        , context : Maybe (Html msg)
        , items : List (Item msg)
        , footer : Maybe (Html msg)
        , model : Model
        , toMsg : Msg -> msg
        }
    -> Html msg
toHtmlWithWords words config =
    let
        (Model model) =
            config.model

        collapsed =
            Resizer.isCollapsed model.resizer
    in
    nav
        [ classList
            [ ( "ui-side-nav", True )

            -- The preference. The stylesheet can also put the navigation into
            -- the rail on its own — below a width where an expanded sidebar
            -- would take more of the screen than the page it frames — which is
            -- why everything the rail needs is rendered either way, and which
            -- of the two is showing is the stylesheet's to decide.
            , ( "is-collapsed", collapsed )
            , ( "is-dragging", Resizer.isDragging model.resizer )
            ]
        , attribute "aria-label" config.label
        , style "--ui-side-nav-width" (String.fromFloat (Resizer.widthPx model.resizer) ++ "px")
        ]
        [ -- Outside the scroll region: the logo is where you go to get back to
          -- the start, and a way back that scrolls out of reach is not one.
          div [ class "ui-side-nav__brand" ]
            [ span [ class "ui-side-nav__brand-full" ] [ config.brand ]
            , span [ class "ui-side-nav__brand-compact" ] [ config.brandCompact ]
            ]
        , case config.context of
            Just context_ ->
                -- Outside the scroll region too: what the menu below is the
                -- menu of has to stay in sight however long that menu is.
                div [ class "ui-side-nav__context" ] [ context_ ]

            Nothing ->
                text ""
        , div [ class "ui-side-nav__scroll" ] (viewSections words config.items)
        , div [ class "ui-side-nav__foot" ]
            (Maybe.values
                [ Maybe.map (\footer -> div [ class "ui-side-nav__footer" ] [ footer ]) config.footer
                , Just (viewCollapseToggle words config.toMsg collapsed)
                ]
            )
        , div [ class "ui-side-nav__resizer" ]
            [ Html.map config.toMsg
                (Html.map ResizerMsg
                    (Resizer.viewWithWords
                        { label = words.resize, collapsed = words.collapsed, width = words.width }
                        identity
                        model.resizer
                    )
                )
            ]
        ]


{-| What the navigation below is the navigation of, where an application has
more than one such place — a workspace, or the organization around them — and a
menu of the others (`context` in `toHtml`).

It sits under the brand and over the menu: the brand says which application
this is, and this says where in it the reader is. It is the place's icon and its
name, with a chevron to say it opens — a title that can be pressed rather than a
field or an entry, so it reads as what the entries under it belong to and not as
the first of them. The menu opens below it; what goes in the menu is the
application's.
In the rail the name goes and the icon stays, and `label` — the control's
accessible name, which should say both where the reader is and what the control
does ("Physics Department, switch workspace") — says the rest.

-}
context :
    { id : String
    , label : String
    , name : String
    , icon : Html msg
    , items : List (Menu.Item msg)
    , menu : Menu.Model
    , toMsg : Menu.Msg -> msg
    , testId : Maybe String
    }
    -> Html msg
context config =
    Menu.toHtml
        { id = config.id
        , trigger =
            Menu.custom
                { label = config.label
                , class = "ui-side-nav__context-trigger"
                , content =
                    [ span [ class "ui-side-nav__context-icon" ] [ config.icon ]
                    , span [ class "ui-side-nav__context-name" ] [ text config.name ]
                    , span [ class "ui-side-nav__context-chevron" ] [ Icon.toHtml (Icon.solid "fa-chevron-down") ]
                    ]
                }
                |> (case config.testId of
                        Just testId ->
                            Menu.withTriggerTestId testId

                        Nothing ->
                            identity
                   )
        , alignment = Menu.Start
        , items = config.items
        , toMsg = config.toMsg
        }
        config.menu


{-| The signed-in user: a face, a name, a role, and the account menu.

It is here rather than left to the app because the shape is the same in every
one of them and the awkward parts are not the app's problem to solve twice — the
menu opens beside the navigation rather than below it, because below it is the
bottom of the screen, and in the rail the name and role go while the face stays.

-}
user :
    { id : String
    , name : String
    , role : String
    , avatar : Html msg
    , avatarCompact : Html msg
    , items : List (Menu.Item msg)
    , menu : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
user config =
    Menu.toHtml
        { id = config.id
        , trigger =
            Menu.custom
                { label = config.name ++ ", " ++ config.role
                , class = "ui-side-nav__user"
                , content =
                    [ span [ class "ui-side-nav__user-avatar" ]
                        [ span [ class "ui-side-nav__user-avatar-full" ] [ config.avatar ]
                        , span [ class "ui-side-nav__user-avatar-compact" ] [ config.avatarCompact ]
                        ]
                    , viewIdentity config
                    ]
                }
        , alignment = Menu.SideEnd
        , items =
            -- The rail hides the name and role, so the menu says them instead —
            -- otherwise the only way to find out whose account this is would be
            -- to open something from it. Always present, and only shown in the
            -- rail, because whether this is the rail is the stylesheet's call.
            Menu.note
                [ span [ class "ui-side-nav__user ui-side-nav__user--note" ]
                    [ span [ class "ui-side-nav__user-avatar" ] [ config.avatar ]
                    , viewIdentity config
                    ]
                ]
                :: Menu.separator
                :: config.items
        , toMsg = config.toMsg
        }
        config.menu


viewIdentity : { a | name : String, role : String } -> Html msg
viewIdentity config =
    span [ class "ui-side-nav__user-text" ]
        [ span [ class "ui-side-nav__user-name" ] [ text config.name ]
        , span [ class "ui-side-nav__user-role" ] [ text config.role ]
        ]


viewCollapseToggle : Words -> (Msg -> msg) -> Bool -> Html msg
viewCollapseToggle words toMsg collapsed =
    let
        ( icon, label ) =
            if collapsed then
                ( "fa-angles-right", words.expand )

            else
                ( "fa-angles-left", words.collapse )
    in
    div [ class "ui-side-nav__toggle" ]
        [ railTip label <|
            button
                [ class "ui-side-nav__collapse"
                , type_ "button"

                -- Controls the width of the navigation it sits in, so the state
                -- belongs on the button rather than on anything it points at.
                , attribute "aria-expanded"
                    (if collapsed then
                        "false"

                     else
                        "true"
                    )
                , Html.Events.onClick (toMsg (setCollapsed (not collapsed)))
                ]
                [ span [ class "ui-side-nav__icon" ] [ Icon.toHtml (Icon.solid icon) ]

                -- Present in both states. Hidden visually in the rail, where it
                -- is the button's only name.
                , span [ class "ui-side-nav__label" ] [ text label ]
                ]
        ]


{-| A button at the foot of the navigation that opens and closes a panel beside
the page — a `Ui.Drawer` — for the application's `footer`. It is drawn as the
collapse toggle is, an icon with its name beside it and the name in a tooltip
in the rail, because it is the same kind of control: it changes the frame, not
the page.

It says whether the panel is open with `aria-expanded`, and, while it is,
which element that is with `aria-controls` — not while it is closed, when the
panel is not in the document to point at.

-}
panelToggle :
    { id : String
    , icon : Icon
    , label : String
    , open : Bool
    , controls : String
    , onToggle : msg
    }
    -> Html msg
panelToggle config =
    railTip config.label <|
        button
            ([ class "ui-side-nav__action"
             , id config.id
             , type_ "button"
             , attribute "aria-expanded"
                (if config.open then
                    "true"

                 else
                    "false"
                )
             , Html.Events.onClick config.onToggle
             ]
                ++ (if config.open then
                        [ attribute "aria-controls" config.controls ]

                    else
                        []
                   )
            )
            [ span [ class "ui-side-nav__icon" ] [ Icon.toHtml config.icon ]
            , span [ class "ui-side-nav__label" ] [ text config.label ]
            ]


{-| A tooltip that only the rail shows.

Rendered whether or not the navigation is collapsed, and hidden by the
stylesheet everywhere but the rail: the rail can be the user's choice or the
viewport's, and only the second of those is known where the stylesheet is.

`Tooltip.view` rather than `viewDescribing`: the label is already in the DOM as
the control's accessible name, so describing it as well would have a screen
reader say the same words twice. The bubble is for the people who can see the
icon and cannot see the name.

-}
railTip : String -> Html msg -> Html msg
railTip label content =
    Tooltip.view { label = label, placement = Tooltip.Trailing } content


{-| The items, with a heading starting a new group.

Each group is its own list inside a `role="group"` named by its heading, so the
rules that address `.ui-side-nav__list > .ui-side-nav__item` reach grouped items
exactly as they reach the rest. Items before the first heading are a plain list.

-}
viewSections : Words -> List (Item msg) -> List (Html msg)
viewSections words items =
    let
        step candidate ( sections, current ) =
            case candidate of
                Heading label ->
                    ( current :: sections, ( Just label, [] ) )

                SectionBreak ->
                    ( current :: sections, ( Nothing, [] ) )

                Item _ ->
                    ( sections, Tuple.mapSecond ((::) candidate) current )

        ( done, last ) =
            List.foldl step ( [], ( Nothing, [] ) ) items

        viewSection index ( maybeHeading, reversed ) =
            let
                list =
                    ul [ class "ui-side-nav__list" ] (List.map (viewItem words 0) (List.reverse reversed))
            in
            case maybeHeading of
                Just label ->
                    let
                        headingId =
                            "ui-side-nav-heading-" ++ String.fromInt index
                    in
                    Just
                        (div
                            [ class "ui-side-nav__section"
                            , attribute "role" "group"
                            , attribute "aria-labelledby" headingId
                            ]
                            [ span [ class "ui-side-nav__heading", id headingId ] [ text label ]
                            , list
                            ]
                        )

                Nothing ->
                    if List.isEmpty reversed then
                        Nothing

                    else
                        Just list
    in
    (last :: done)
        |> List.reverse
        |> List.indexedMap viewSection
        |> Maybe.values


{-| A group you are inside becomes a box: the links under it are the box's
contents, and the page you are on is the one highlighted within it.

Every other group is folded. Its link goes to the group's first page, and its
children become a flyout beside the navigation, with the group's name at the top
of it — the same flyout every group has in the rail, so the subpages of a section
you are not in are one hover or one Tab away without unfolding the navigation
around the page you are on. A chevron after a folded group's name says there is
more behind it. It opens on hover and on focus, and because
the children follow the group in the document, Tab walks from the group straight
into them without anything being written to make it so.

-}
viewItem : Words -> Int -> Item msg -> Html msg
viewItem words depth candidate =
    case candidate of
        Item entry ->
            viewEntry words depth entry

        _ ->
            -- A heading or a break is only reachable here inside a group's
            -- children, where there is no list of its own to start. Drawn as
            -- nothing rather than as text between links.
            text ""


viewEntry : Words -> Int -> Entry msg -> Html msg
viewEntry words depth entry =
    let
        childActive =
            List.any isActive entry.children

        hasChildren =
            not (List.isEmpty entry.children)

        -- A group whose child is the current page, or which is the current page
        -- and has children of its own.
        isOpen =
            hasChildren && (entry.active || childActive)

        -- Only a link without children is ever the page. An active group with
        -- no active child is a page inside the section none of its links name —
        -- a knowledge model's detail under "Knowledge Models" — so the group is
        -- open and tinted as the box, and no row within it is picked out: the
        -- group's own link is the first row of that box and goes to the list,
        -- which is not where you are.
        isCurrent =
            entry.active && not hasChildren

        link =
            Html.a
                (href entry.url
                    :: classList
                        [ ( "ui-side-nav__link", True )

                        -- The tint marks the page itself. A group never takes
                        -- it: holding the page, there would be two of them, and
                        -- being the page, see `isCurrent`.
                        , ( "is-active", isCurrent )
                        , ( "is-within", isOpen )
                        , ( "is-nested", depth > 0 )
                        ]
                    -- The same rule for the announcement: one current page.
                    :: (if isCurrent then
                            [ attribute "aria-current" "page" ]

                        else
                            []
                       )
                    ++ (if entry.newTab then
                            [ target "_blank", rel "noopener noreferrer" ]

                        else
                            []
                       )
                    ++ (case entry.id of
                            Just itemId ->
                                [ id itemId ]

                            Nothing ->
                                []
                       )
                    ++ TestId.attributes entry.testId
                )
                (Maybe.values
                    [ Maybe.map (\icon -> span [ class "ui-side-nav__icon" ] [ Icon.toHtml icon ]) entry.icon
                    , Just (span [ class "ui-side-nav__label" ] [ text entry.label ])
                    , Maybe.map (\badge -> span [ class "ui-side-nav__badge" ] [ badge ]) entry.badge
                    , if hasChildren then
                        -- Decorative: the flyout it points to is reached by Tab
                        -- and announced as the links it holds.
                        Just (span [ class "ui-side-nav__chevron", attribute "aria-hidden" "true" ] [ Icon.toHtml (Icon.solid "fa-angle-right") ])

                      else
                        Nothing
                    , if entry.newTab then
                        Just (viewNewTab (Maybe.withDefault words.newTab entry.externalNotice))

                      else
                        Nothing
                    ]
                )
    in
    li
        [ classList
            [ ( "ui-side-nav__item", True )
            , ( "is-open", isOpen )
            , ( "has-children", hasChildren )
            ]
        ]
        (if hasChildren then
            -- No rail tooltip on a group: its flyout carries its name, and a
            -- tooltip beside a flyout would be two things saying one word.
            [ link
            , ul [ class "ui-side-nav__sublist" ]
                (li [ class "ui-side-nav__flyout-heading", attribute "aria-hidden" "true" ] [ text entry.label ]
                    :: List.map (viewItem words (depth + 1)) entry.children
                )
            ]

         else if depth == 0 then
            [ railTip entry.label link ]

         else
            [ link ]
        )


{-| The icon a sighted reader sees and the words everyone else hears.
-}
viewNewTab : String -> Html msg
viewNewTab notice =
    span [ class "ui-side-nav__external" ]
        [ Icon.toHtml (Icon.solid "fa-arrow-up-right-from-square")
        , span [ class "ui-visually-hidden" ] [ text (" " ++ notice) ]
        ]
