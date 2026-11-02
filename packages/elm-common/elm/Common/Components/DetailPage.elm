module Common.Components.DetailPage exposing
    ( DetailPage
    , Section
    , TabItem
    , detailPage
    , resizerConfig
    , section
    , sectionHtml
    , toHtml
    , view
    , withActions
    , withActionsLeading
    , withBadges
    , withFillContent
    , withFlushContent
    , withFooter
    , withHeaderAside
    , withLocale
    , withMoreActions
    , withMoreActionsJoined
    , withSidePanel
    , withTabs
    , withTabsBadged
    , withTabsWithTestIds
    , withTitleIcon
    )

{-| The page of one thing: a knowledge model, a template, a locale, a tenant.

One builder over `Ui.Page.Detail`, so every detail page has the same header,
the same tabs and the same panel, and the decisions below are made once.

**The title is the page's `h1`.** `Ui.Page.Detail` renders it with
`Ui.Text.pageTitle`, so it is where focus lands after a route change. Badges sit
beside it and are part of what the eye reads with the name; they are text, not
color alone.

**Anything else in the header sits at the end of the title's line** —
`withHeaderAside`, a status such as the questionnaire's saving — **or before the
actions**: `withActionsLeading`, the people editing the project right now as a
`Ui.AvatarGroup`, parted from the Share button by a rule, as in the wizard. Both
are for something to look at, not something to press.

**The header's actions are buttons first and a kebab after.** The wizard put
every action in a dropdown, which hides the one thing most people came to do
behind an extra press. `withActions` is for the few that deserve a button;
`withMoreActions` is `Common.Components.ItemActions.menu`, whose trigger is
labeled with whose actions these are. When everything in it is more of the last
button — Share, and how else to share — `withMoreActionsJoined` makes the two
one split button instead.

**Tabs are routes, so they are `Ui.NavTabs`.** Links in a `nav` with
`aria-current`, not a tablist: each one goes to another URL, and a screen reader
announcing "tab, 2 of 5" would describe something the user is about to leave.

**The side panel is facts about the thing, in titled sections.** A complementary
landmark (so it can be skipped), resizable with `Ui.Resizer` from the keyboard as
well as by dragging, and made of `Ui.DescriptionList`s — a real `dl`, label
above value, so a long value never ends up in a gutter. Each section is a card with a heading, so a screen reader's heading
list reaches "Organization" or "Other versions" directly. Below 48rem the panel
follows the content and both scroll as one under the header; that is the
template's, and nothing here has to know.

-}

import ActionResult exposing (ActionResult)
import Common.Components.ItemActions as ItemActions
import Common.Components.Page as Page
import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Badge as Badge exposing (Badge)
import Ui.Button exposing (Button)
import Ui.Card as Card
import Ui.DescriptionList as DescriptionList
import Ui.Icon exposing (Icon)
import Ui.Menu as Menu
import Ui.NavTabs as NavTabs
import Ui.Page.Detail as Detail
import Ui.Resizer as Resizer


type DetailPage msg
    = DetailPage
        { title : String
        , content : List (Html msg)
        , badges : List (Badge msg)
        , actions : List (Button msg)
        , moreActions : Maybe (MoreActions msg)
        , moreActionsJoined : Maybe String
        , tabs : Maybe Tabs
        , sidePanel : Maybe (SidePanel msg)
        , resizerWords : Maybe Resizer.Words
        , titleIcon : Maybe (Html msg)
        , headerAside : Maybe (Html msg)
        , actionsLeading : Maybe (Html msg)
        , flushContent : Bool
        , fillContent : Bool
        , footer : Maybe (Html msg)
        }


type alias MoreActions msg =
    { id : String
    , label : String
    , actions : List (ItemActions.Action msg)
    , model : Menu.Model
    , toMsg : Menu.Msg -> msg
    }


type alias Tabs =
    { label : String
    , items : List TabItem
    }


type alias TabItem =
    { label : String, url : String, active : Bool, badge : Maybe String, testId : Maybe String, icon : Maybe Icon }


type alias SidePanel msg =
    { resizer : Resizer.Model
    , toMsg : Resizer.Msg -> msg
    , sections : List (Section msg)
    }


{-| A titled block in the side panel.
-}
type Section msg
    = Section { title : String, content : List (Html msg) }


detailPage : { title : String, content : List (Html msg) } -> DetailPage msg
detailPage config =
    DetailPage
        { title = config.title
        , content = config.content
        , badges = []
        , actions = []
        , moreActions = Nothing
        , moreActionsJoined = Nothing
        , tabs = Nothing
        , sidePanel = Nothing
        , resizerWords = Nothing
        , titleIcon = Nothing
        , headerAside = Nothing
        , actionsLeading = Nothing
        , flushContent = False
        , fillContent = False
        , footer = Nothing
        }


{-| A picture before the title — the user's avatar, the tenant's logo, usually
a `Common.Components.UserAvatar`. Decorative; see `Ui.Page.Detail.withTitleIcon`.
-}
withTitleIcon : Html msg -> DetailPage msg -> DetailPage msg
withTitleIcon icon (DetailPage config) =
    DetailPage { config | titleIcon = Just icon }


{-| The people editing the thing now, before the actions and apart from them by
a rule; see `Ui.Page.Detail.withActionsLeading`.
-}
withActionsLeading : Html msg -> DetailPage msg -> DetailPage msg
withActionsLeading leading (DetailPage config) =
    DetailPage { config | actionsLeading = Just leading }


{-| Something to see at the end of the title's line, after the badges: a saving
indicator. It says something about the thing whose name it follows, so it is read
with the title rather than with the content.

Not a place for actions — see `withActions` and `withMoreActions`. Whatever
goes here names itself.

-}
withHeaderAside : Html msg -> DetailPage msg -> DetailPage msg
withHeaderAside aside (DetailPage config) =
    DetailPage { config | headerAside = Just aside }


{-| Status next to the title: "Published", "default", the version.
-}
withBadges : List (Badge msg) -> DetailPage msg -> DetailPage msg
withBadges badges (DetailPage config) =
    DetailPage { config | badges = badges }


{-| The actions that deserve a button — usually one or two. Everything else goes
in `withMoreActions`.
-}
withActions : List (Button msg) -> DetailPage msg -> DetailPage msg
withActions actions (DetailPage config) =
    DetailPage { config | actions = actions }


{-| The overflow menu at the end of the header.

The label is the trigger's accessible name, and should say whose actions these
are: "Actions for Data management plan". Nothing is rendered when every action
was filtered out by `ItemActions.groups` — a menu that opens onto nothing is a
tab stop that lies.

-}
withMoreActions :
    { id : String
    , label : String
    , actions : List (ItemActions.Action msg)
    , model : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> DetailPage msg
    -> DetailPage msg
withMoreActions moreActions (DetailPage config) =
    DetailPage { config | moreActions = Just moreActions }


{-| Join the more actions to the last button as a split button — the button,
and a chevron beside it opening the rest — instead of a kebab at the end of the
header. For a menu that holds more of that one action, the way Share's holds
"Copy link" and who can reach the project.

The string names the pair, as a `Ui.ButtonGroup`: "Sharing". The menu's own
label becomes the chevron's name, so it should say what the menu holds —
"Sharing options" — rather than whose actions these are.

-}
withMoreActionsJoined : String -> DetailPage msg -> DetailPage msg
withMoreActionsJoined label (DetailPage config) =
    DetailPage { config | moreActionsJoined = Just label }


{-| Navigation between the sections of the thing, each its own route.

`label` names the navigation landmark, typically the kind of thing: "Knowledge
model". A single tab is not a navigation, so it is not rendered.

-}
withTabs :
    { label : String
    , items : List { label : String, url : String, active : Bool }
    }
    -> DetailPage msg
    -> DetailPage msg
withTabs tabs (DetailPage config) =
    DetailPage
        { config
            | tabs =
                Just
                    { label = tabs.label
                    , items =
                        List.map
                            (\entry -> { label = entry.label, url = entry.url, active = entry.active, badge = Nothing, testId = Nothing, icon = Nothing })
                            tabs.items
                    }
        }


{-| `withTabs`, where a tab can carry a count or a short status after its label
— "Locales 3". The badge is part of the link, so it is read with the tab's name,
and it is text, not a dot. Pass the number as words the page has formatted; a
tab with nothing to count takes `Nothing`, not `Just "0"`, unless no items is
itself worth saying.
-}
withTabsBadged :
    { label : String
    , items : List { label : String, url : String, active : Bool, badge : Maybe String }
    }
    -> DetailPage msg
    -> DetailPage msg
withTabsBadged tabs (DetailPage config) =
    DetailPage
        { config
            | tabs =
                Just
                    { label = tabs.label
                    , items =
                        List.map
                            (\entry -> { label = entry.label, url = entry.url, active = entry.active, badge = entry.badge, testId = Nothing, icon = Nothing })
                            tabs.items
                    }
        }


{-| `withTabsBadged`, where a tab can also carry a picture of what it leads to
and a `data-cy` for the end-to-end tests.

The icon is decoration: the tab's own words are its name, and a row of tabs
that needed its pictures read out would be a row whose words were not enough.

-}
withTabsWithTestIds :
    { label : String
    , items : List TabItem
    }
    -> DetailPage msg
    -> DetailPage msg
withTabsWithTestIds tabs (DetailPage config) =
    DetailPage { config | tabs = Just tabs }


{-| The tab is one surface, edge to edge — a document preview in a frame, the
questionnaire: no padding round it, the whole height under the tabs, and its
own scrolling instead of the page's. Off by default. See
`Ui.Page.Detail.withFlushContent`.

Turn it on for what the tab shows, not for the tab: while the same tab is
loading or has something to say, it is a message that needs room round it, and
the body stays padded.

-}
withFlushContent : Bool -> DetailPage msg -> DetailPage msg
withFlushContent flushContent (DetailPage config) =
    DetailPage { config | flushContent = flushContent }


{-| The content takes the whole width and height and keeps its padding, for a
page that is there to show one long thing — a log, a request's body — in a
`Ui.CodeBlock.fill`. See `Ui.Page.Detail.withFillContent`.
-}
withFillContent : DetailPage msg -> DetailPage msg
withFillContent (DetailPage config) =
    DetailPage { config | fillContent = True }


{-| A band below the tab's scrolling content, for `Common.Components.Form.unsavedBar`
on a tab that edits the thing — the project's settings — as the settings pages
have one. Off by default. See `Ui.Page.Detail.withFooter`.

Pass it for the tab that has something to save and for no other: the footer
belongs to what the tab shows, not to the page.

-}
withFooter : Html msg -> DetailPage msg -> DetailPage msg
withFooter footer (DetailPage config) =
    DetailPage { config | footer = Just footer }


{-| The panel of facts down the end edge. Sections with nothing in them are
dropped, and a panel with no sections is not rendered.
-}
withSidePanel :
    { resizer : Resizer.Model
    , toMsg : Resizer.Msg -> msg
    , sections : List (Section msg)
    }
    -> DetailPage msg
    -> DetailPage msg
withSidePanel sidePanel (DetailPage config) =
    DetailPage { config | sidePanel = Just sidePanel }


{-| Label-and-value facts under a title.
-}
section : String -> List (DescriptionList.Entry msg) -> Section msg
section title entries =
    Section
        { title = title
        , content =
            if List.isEmpty entries then
                []

            else
                [ DescriptionList.toHtml entries ]
        }


{-| Anything that is not a list of facts — other versions as a list of links,
an organization with its logo. Pass an empty list to drop the section.
-}
sectionHtml : String -> List (Html msg) -> Section msg
sectionHtml title content =
    Section { title = title, content = content }


{-| The side panel's resizer, the same for every detail page so a width stored
from one fits all of them.

Initialize the page's `Resizer.Model` with `Resizer.init (resizerConfig
appState)`, and `Resizer.restore` a stored width if the app keeps one; persist on
`Resizer.isDragging model == False`, not on every pointer move.

-}
resizerConfig : WithLocale a -> Resizer.Config
resizerConfig appState =
    { edge = Resizer.Leading
    , label = gettext "Resize panel" appState.locale
    , min = 240
    , max = 520
    , default = 320
    , collapse = Nothing
    }


{-| Says what the page says of its own accord — the side panel's separator, its
name and its width — in the language of the page. `view` does this itself; a
page rendered with `toHtml` should call it.
-}
withLocale : WithLocale a -> DetailPage msg -> DetailPage msg
withLocale appState (DetailPage config) =
    DetailPage { config | resizerWords = Just (UiWords.resizer appState (gettext "Resize panel" appState.locale)) }


toHtml : DetailPage msg -> Html msg
toHtml (DetailPage config) =
    Detail.detail { title = config.title, content = config.content }
        |> applyMaybe identity Detail.withSidePanelWords config.resizerWords
        |> applyMaybe identity Detail.withTitleIcon config.titleIcon
        |> applyMaybe identity Detail.withTitleAside config.headerAside
        |> applyMaybe identity Detail.withActionsLeading config.actionsLeading
        |> Detail.withBadges config.badges
        |> Detail.withActions config.actions
        |> applyMaybe (viewMoreActions config.moreActionsJoined) Detail.withMenu (Maybe.andThen nonEmptyActions config.moreActions)
        |> applyMaybe identity Detail.withMenuJoined config.moreActionsJoined
        |> applyMaybe viewTabs Detail.withNav (Maybe.andThen nonTrivialTabs config.tabs)
        |> applySidePanel config.sidePanel
        |> applyWhen config.flushContent Detail.withFlushContent
        |> applyWhen config.fillContent Detail.withFillContent
        |> applyMaybe identity Detail.withFooter config.footer
        |> Detail.toHtml


{-| The page for a request: loading, the error, or the page itself, through
`Common.Components.Page.actionResult`.

While loading there is no title yet, so there is no `h1`; the loading region
carries a visible label. An error is a full-page message whose title is the
`h1`, so focus after the route change still lands on something that says what
happened.

-}
view : WithLocale a -> (x -> DetailPage msg) -> ActionResult x -> Html msg
view appState toDetailPage =
    Page.actionResult appState (toHtml << withLocale appState << toDetailPage)


applyMaybe : (a -> b) -> (b -> Detail.Detail msg -> Detail.Detail msg) -> Maybe a -> Detail.Detail msg -> Detail.Detail msg
applyMaybe render apply maybeValue detail =
    case maybeValue of
        Just value ->
            apply (render value) detail

        Nothing ->
            detail


applyWhen : Bool -> (Detail.Detail msg -> Detail.Detail msg) -> Detail.Detail msg -> Detail.Detail msg
applyWhen condition apply detail =
    if condition then
        apply detail

    else
        detail


viewMoreActions : Maybe String -> MoreActions msg -> Html msg
viewMoreActions joined =
    case joined of
        Just _ ->
            ItemActions.splitMenu

        Nothing ->
            ItemActions.menu


nonEmptyActions : MoreActions msg -> Maybe (MoreActions msg)
nonEmptyActions moreActions =
    if List.isEmpty moreActions.actions then
        Nothing

    else
        Just moreActions


nonTrivialTabs : Tabs -> Maybe Tabs
nonTrivialTabs tabs =
    if List.length tabs.items > 1 then
        Just tabs

    else
        Nothing


viewTabs : Tabs -> Html msg
viewTabs tabs =
    NavTabs.toHtml
        { label = tabs.label
        , full = False
        , items = List.map viewTab tabs.items
        }


viewTab : TabItem -> NavTabs.Item msg
viewTab entry =
    let
        tab =
            NavTabs.item { label = entry.label, url = entry.url, active = entry.active }
                |> (case entry.icon of
                        Just icon ->
                            NavTabs.withIcon icon

                        Nothing ->
                            identity
                   )
                |> (case entry.testId of
                        Just testId ->
                            NavTabs.withTestId testId

                        Nothing ->
                            identity
                   )
    in
    case entry.badge of
        Just badge ->
            NavTabs.withBadge (Badge.toHtml (Badge.neutral badge)) tab

        Nothing ->
            tab


applySidePanel : Maybe (SidePanel msg) -> Detail.Detail msg -> Detail.Detail msg
applySidePanel maybeSidePanel detail =
    case maybeSidePanel of
        Just sidePanel ->
            let
                sections =
                    List.filterMap viewSection sidePanel.sections
            in
            if List.isEmpty sections then
                detail

            else
                Detail.withSidePanel { resizer = sidePanel.resizer, toMsg = sidePanel.toMsg } sections detail

        Nothing ->
            detail


viewSection : Section msg -> Maybe (Html msg)
viewSection (Section config) =
    if List.isEmpty config.content then
        Nothing

    else
        -- A card with a heading: the terms inside are labels, and a heading
        -- above them at the label's size would be two things at one level.
        Just (Card.viewWithTitle config.title config.content)
