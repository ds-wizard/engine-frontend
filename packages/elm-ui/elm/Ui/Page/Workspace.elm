module Ui.Page.Workspace exposing
    ( Aside
    , Nav
    , Toolbar
    , Workspace
    , asideId
    , mainId
    , navId
    , navToggleId
    , toHtml
    , withAside
    , withFlushMain
    , withFlushNavHeader
    , withNav
    , withNavToggleExtras
    , withToolbar
    , workspace
    )

{-| A place to work in: a navigation down the start, the work in the middle, and
a panel down the end — the shape of an editor. The
questionnaire is its first user; the knowledge model and document template
editors are the next.

It fills the region it is given and never scrolls as a whole. The navigation, the
work and the panel each scroll on their own, so a long chapter list, a long
chapter and a long panel do not take each other along. The work's scroll
container has a known id (`mainId`) for the app to scroll, restore and focus in.

**Resizable.** The navigation and the panel each carry a `Ui.Resizer` on their
inner edge: widths are the reader's to choose, and the models are the app's to
keep.

**Closing the panel** is the application's, not the panel's: the control that
opened it is the control that closes it, and while the panel is beside the work
that control is still on the toolbar, still pressed. The panel draws a close
button only where it is over the work rather than beside it.

**Narrow: overlays.** Below the point where the three do not fit side by side —
measured on the workspace, not the window, since the app's own navigation takes
its share — the work takes the whole width. The navigation becomes an overlay
opened by a button the workspace puts at the start of the toolbar ("Chapters"),
and the panel, when there is one, opens over the work from the end edge. Both
close with Escape, with their own close button, and with a press on the work
behind them. The overlays are not modal: the page behind stays reachable, which
is why the state is a disclosure (`aria-expanded` on the toggle) and not a
dialog. The app moves focus into the navigation when it opens it
(`navId`) and back to the toggle when it closes it (`navToggleId`), since only
the app can issue the command.

The open state of the narrow navigation is the app's (`Nav.overlayOpen`). It is
ignored when the workspace is wide, where the navigation is simply there — so
the app never needs to know which layout is showing.

**The toolbar** is a row of the work's controls above the three regions: the
app's controls at the start and the end, and the navigation's toggle before
them. It is a named group for the same reason `Ui.Toolbar` is one.

-}

import Html exposing (Html, aside, button, div, h2, nav, span, text)
import Html.Attributes exposing (attribute, class, classList, id, style, tabindex, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Ui.Icon as Icon
import Ui.Resizer as Resizer


type Workspace msg
    = Workspace
        { id : String
        , main : List (Html msg)
        , toolbar : Maybe (Toolbar msg)
        , nav : Maybe (Nav msg)
        , aside : Maybe (Aside msg)
        , flushMain : Bool
        , flushNavHeader : Bool
        , navToggleExtras : List (Html msg)
        }


type alias Toolbar msg =
    { label : String
    , start : List (Html msg)
    , end : List (Html msg)
    }


{-| The navigation down the start.

  - `label` names the landmark and is shown above the content when it is an
    overlay, beside its close button;
  - `header` stays at the top while `content` scrolls under it — what the
    navigation is about (a phase, a title), above the list that can be long;
  - `overlayOpen` and `onOverlay` are the narrow layout's disclosure;
  - `toggleLabel` is what the toolbar's button says ("Chapters"), and
    `closeLabel` what the overlay's close button is named.

-}
type alias Nav msg =
    { label : String
    , header : List (Html msg)
    , content : List (Html msg)
    , resizer : Resizer.Model
    , toResizerMsg : Resizer.Msg -> msg
    , resizerWords : Resizer.Words
    , overlayOpen : Bool
    , onOverlay : Bool -> msg
    , toggleLabel : String
    , closeLabel : String
    }


{-| The panel down the end: a complementary region named by its `title`, which
the workspace draws as the panel's heading beside a close button. Present only
while there is a panel to show — closing it is the app dropping it.
-}
type alias Aside msg =
    { title : String
    , content : List (Html msg)
    , resizer : Resizer.Model
    , toResizerMsg : Resizer.Msg -> msg
    , resizerWords : Resizer.Words
    , onClose : msg
    , closeLabel : String

    -- True when nothing outside the panel closes it. Beside the work a panel
    -- normally has no close button, because the toolbar control that opened it
    -- is still there and still pressed; a panel opened from inside the work —
    -- a row of a table that scrolls away — has no such control, so it draws
    -- one of its own at every width.
    , closeInPanel : Bool
    }


{-| `id` is the workspace's, and the ids of its parts are made from it
(`mainId`, `navId`, `navToggleId`, `asideId`).
-}
workspace : { id : String, main : List (Html msg) } -> Workspace msg
workspace config =
    Workspace
        { id = config.id
        , main = config.main
        , toolbar = Nothing
        , nav = Nothing
        , aside = Nothing
        , flushMain = False
        , flushNavHeader = False
        , navToggleExtras = []
        }


withToolbar : Toolbar msg -> Workspace msg -> Workspace msg
withToolbar toolbar (Workspace config) =
    Workspace { config | toolbar = Just toolbar }


withNav : Nav msg -> Workspace msg -> Workspace msg
withNav navigation (Workspace config) =
    Workspace { config | nav = Just navigation }


{-| The work takes the whole width it is given, with no gutter of its own.

For content that is edge to edge by nature — a table whose columns run to the
sides, where a gutter reads as a margin round the table rather than as room to
breathe.

-}
withFlushMain : Workspace msg -> Workspace msg
withFlushMain (Workspace config) =
    Workspace { config | flushMain = True }


{-| A navigation header that is a bar of its own rather than something set into
the panel: it takes the panel's full width and brings its own padding.

For a header that is a row of controls with a line under it — the file list's
toolbar in the document template editor — where the panel's own inset would
leave that line short of both edges and the row floating in the middle of it.

-}
withFlushNavHeader : Workspace msg -> Workspace msg
withFlushNavHeader (Workspace config) =
    Workspace { config | flushNavHeader = True }


{-| Controls beside the navigation's own toggle, in the row that holds it.

For a workspace whose work is another workspace — the project importer, whose
changes are this one's navigation and whose questionnaire brings a column of its
own. Both columns fold away on a narrow screen, and two rows with one button each
is two rows; the control for the inner one belongs in the row that already exists.

The row is only there while the navigation is an overlay, so what is put in it
should be too.

-}
withNavToggleExtras : List (Html msg) -> Workspace msg -> Workspace msg
withNavToggleExtras extras (Workspace config) =
    Workspace { config | navToggleExtras = extras }


withAside : Aside msg -> Workspace msg -> Workspace msg
withAside panel (Workspace config) =
    Workspace { config | aside = Just panel }


{-| The work's scroll container.
-}
mainId : String -> String
mainId workspaceId =
    workspaceId ++ "-main"


navId : String -> String
navId workspaceId =
    workspaceId ++ "-nav"


navToggleId : String -> String
navToggleId workspaceId =
    workspaceId ++ "-nav-toggle"


asideId : String -> String
asideId workspaceId =
    workspaceId ++ "-aside"


toHtml : Workspace msg -> Html msg
toHtml (Workspace config) =
    let
        navOpen =
            Maybe.map .overlayOpen config.nav == Just True

        -- Behind an open overlay, and only ever shown when narrow: a press on
        -- the work closes what is over it, the navigation first.
        scrim =
            case ( config.nav, config.aside ) of
                ( Just navigation, _ ) ->
                    if navigation.overlayOpen then
                        [ viewScrim (navigation.onOverlay False) ]

                    else
                        Maybe.unwrap [] (\panel -> [ viewScrim panel.onClose ]) config.aside

                ( Nothing, Just panel ) ->
                    [ viewScrim panel.onClose ]

                ( Nothing, Nothing ) ->
                    []
    in
    div [ class "ui-workspace", id config.id ]
        [ div
            [ classList
                [ ( "ui-workspace__frame", True )
                , ( "has-nav", Maybe.isJust config.nav )
                , ( "has-aside", Maybe.isJust config.aside )
                , ( "is-nav-open", navOpen )
                ]
            ]
            (viewToolbar config
                ++ Maybe.unwrap [] (\navigation -> [ viewNav config.id config.flushNavHeader navigation ]) config.nav
                ++ [ div
                        [ classList
                            [ ( "ui-workspace__main", True )
                            , ( "is-flush", config.flushMain )
                            ]
                        , id (mainId config.id)
                        , tabindex -1
                        ]
                        config.main
                   ]
                ++ scrim
                ++ Maybe.unwrap [] (\panel -> [ viewAside config.id panel ]) config.aside
            )
        ]


viewToolbar : { a | id : String, toolbar : Maybe (Toolbar msg), nav : Maybe (Nav msg), navToggleExtras : List (Html msg) } -> List (Html msg)
viewToolbar config =
    let
        toggle =
            case config.nav of
                Just navigation ->
                    [ button
                        [ type_ "button"
                        , class "ui-button ui-button--ghost ui-workspace__nav-toggle"
                        , id (navToggleId config.id)
                        , attribute "aria-expanded" (boolString navigation.overlayOpen)
                        , attribute "aria-controls" (navId config.id)
                        , onClick (navigation.onOverlay (not navigation.overlayOpen))
                        ]
                        [ Icon.toHtml (Icon.solid "fa-bars"), span [] [ text navigation.toggleLabel ] ]
                    ]

                Nothing ->
                    []
    in
    case config.toolbar of
        Just toolbar ->
            [ div [ class "ui-workspace__toolbar", attribute "role" "group", attribute "aria-label" toolbar.label ]
                [ div [ class "ui-workspace__toolbar-start" ] (toggle ++ config.navToggleExtras ++ toolbar.start)
                , div [ class "ui-workspace__toolbar-end" ] toolbar.end
                ]
            ]

        Nothing ->
            if List.isEmpty toggle then
                []

            else
                -- No toolbar of the app's: a row for the toggle alone, which
                -- exists only when the navigation is an overlay.
                [ div [ class "ui-workspace__toolbar ui-workspace__toolbar--nav-only" ]
                    [ div [ class "ui-workspace__toolbar-start" ] (toggle ++ config.navToggleExtras) ]
                ]


viewNav : String -> Bool -> Nav msg -> Html msg
viewNav workspaceId flushHeader navigation =
    nav
        [ classList [ ( "ui-workspace__nav", True ), ( "is-open", navigation.overlayOpen ) ]
        , id (navId workspaceId)
        , attribute "aria-label" navigation.label
        , tabindex -1
        , style "--ui-workspace-nav-width" (String.fromFloat (Resizer.widthPx navigation.resizer) ++ "px")
        , onEscape (navigation.onOverlay False) navigation.overlayOpen
        ]
        [ div [ class "ui-workspace__overlay-header" ]
            [ span [ class "ui-workspace__overlay-title" ] [ text navigation.label ]
            , closeButton navigation.closeLabel (navigation.onOverlay False)
            ]
        , if List.isEmpty navigation.header then
            text ""

          else
            div
                [ classList
                    [ ( "ui-workspace__nav-header", True )
                    , ( "is-flush", flushHeader )
                    ]
                ]
                navigation.header
        , div [ class "ui-workspace__nav-scroll" ] navigation.content
        , Resizer.viewWithWords navigation.resizerWords navigation.toResizerMsg navigation.resizer
        ]


viewAside : String -> Aside msg -> Html msg
viewAside workspaceId panel =
    let
        headingId =
            asideId workspaceId ++ "-title"
    in
    aside
        [ class "ui-workspace__aside"
        , id (asideId workspaceId)
        , attribute "aria-labelledby" headingId
        , tabindex -1
        , style "--ui-workspace-aside-width" (String.fromFloat (Resizer.widthPx panel.resizer) ++ "px")
        ]
        [ -- The panel's name is not drawn: the control that opened it is in the
          -- toolbar a few pixels above, pressed and saying the same word, and a
          -- panel this narrow has better uses for the line. It stays in the page
          -- for everyone who is not looking at it — it names the landmark, and
          -- as a heading it is a way in to the panel for a reader moving by
          -- headings.
          h2 [ class "ui-workspace__aside-title", id headingId ] [ text panel.title ]

        -- Beside the work, the way out is the control that opened it, which is
        -- still there and still pressed. Over the work it is not obvious that
        -- pressing it again is the way back, so the panel offers one of its own
        -- — the narrow layout draws this row, and the wide one does not.
        , div
            [ classList
                [ ( "ui-workspace__aside-close", True )
                , ( "is-always-shown", panel.closeInPanel )
                ]
            ]
            [ closeButton panel.closeLabel panel.onClose ]
        , div [ class "ui-workspace__aside-scroll" ] panel.content
        , Resizer.viewWithWords panel.resizerWords panel.toResizerMsg panel.resizer
        ]


closeButton : String -> msg -> Html msg
closeButton label msg =
    button
        [ type_ "button"
        , class "ui-button ui-button--ghost ui-button--icon ui-button--sm"
        , attribute "aria-label" label
        , attribute "data-ui-tooltip" label

        -- It sits at the end of its own edge, where a bubble under the middle
        -- of it runs off the side of the window.
        , attribute "data-ui-tooltip-placement" "end"
        , onClick msg
        ]
        [ Icon.toHtml (Icon.solid "fa-xmark") ]


viewScrim : msg -> Html msg
viewScrim msg =
    -- Pointer-only by design: the keyboard closes an overlay with Escape or its
    -- close button, and a scrim in the tab order would be a stop that says
    -- nothing.
    div [ class "ui-workspace__scrim", attribute "aria-hidden" "true", onClick msg ] []


onEscape : msg -> Bool -> Html.Attribute msg
onEscape msg active =
    Html.Events.custom "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if active && key == "Escape" then
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    else
                        Decode.fail "not an Escape that closes"
                )
        )


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"
