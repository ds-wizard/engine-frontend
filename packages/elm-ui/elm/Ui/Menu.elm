module Ui.Menu exposing
    ( Alignment(..)
    , Item
    , Model
    , Msg
    , Option
    , Trigger
    , checkItem
    , choiceItem
    , close
    , closeWithoutFocus
    , custom
    , dangerItem
    , dangerLinkItem
    , focusableCount
    , header
    , init
    , isOpen
    , item
    , kebab
    , labeled
    , linkItem
    , note
    , quiet
    , radioItem
    , searchInput
    , segmented
    , separator
    , split
    , subscriptions
    , toHtml
    , triggerId
    , update
    , withDescription
    , withExternalNotice
    , withIcon
    , withLeading
    , withNewTab
    , withShortcut
    , withTestId
    , withTriggerTestId
    , withTriggerTooltipAt
    )

{-| A menu that is one tab stop.

This is the APG menu-button pattern, and it is the single biggest lever on
keyboard usability in the whole system: a page of eight components should be
about ten tab stops, not two hundred. Tab reaches the trigger; the arrow keys,
Home, End and Escape move within.

It cannot be retrofitted per page, which is the argument for it living in the
component rather than in a guideline.

-}

import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, tabindex, type_)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode
import Task
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId
import Ui.Tooltip as Tooltip


type Model
    = Model
        { open : Bool
        , focused : Int

        -- Opened towards the other edge, because the list would have stuck
        -- out of the window on its own side (`Placed`).
        , flipped : Bool
        }


{-| Whether the menu is showing its items.

For a caller with something to do when it opens — a list of people to fetch,
which is worth asking for when it is about to be read and not before.

-}
isOpen : Model -> Bool
isOpen (Model model) =
    model.open


{-| How the menu is opened.

`kebab` is the three-dot trigger a list row uses: icon-only and borderless, so a
column of them reads as a column of affordances rather than a column of buttons.
Its label is not optional — it becomes the accessible name.

-}
type Trigger msg
    = Trigger { kind : TriggerKind msg, testId : Maybe String, tooltip : Maybe Tooltip.Placement }


type TriggerKind msg
    = Labeled String
    | Kebab String
    | Split String
    | Custom { label : String, class : String, content : List (Html msg) }


trigger : TriggerKind msg -> Trigger msg
trigger kind =
    Trigger { kind = kind, testId = Nothing, tooltip = Nothing }


labeled : String -> Trigger msg
labeled =
    trigger << Labeled


kebab : String -> Trigger msg
kebab =
    trigger << Kebab


{-| The other half of a split button: a secondary button with only a chevron,
to go after the secondary button it offers more of, joined to it in a
`Ui.ButtonGroup` — Share, and beside it what else there is to do with sharing.

Icon-only, so the label is the accessible name, and it should say what the menu
holds rather than that it is a menu: "Sharing options".

Secondary, because the one split button there is — Share on a project — is not
the page's main action, and a filled button there drew the eye away from the
work the page is for. The two halves' borders overlap into the join, as any
group's do. A primary split, if one is ever wanted, needs a join drawn of its
own: two filled halves have no border to overlap.

-}
split : String -> Trigger msg
split =
    trigger << Split


{-| Which edge of the trigger the menu lines up with.

A menu at the end of a row has to open leftwards or it runs off the page, which
is why this is the caller's choice rather than the component's.

-}
type Alignment
    = Start
    | End
      -- Beside the trigger rather than below it, aligned to its bottom edge.
      -- For a menu at the foot of the navigation, where below is off the
      -- screen and the room is to the side.
    | SideEnd


type Item msg
    = Item
        { label : String
        , icon : Maybe Icon
        , leading : Maybe (Html msg)
        , action : Action msg
        , dangerous : Bool
        , state : State
        , description : Maybe String
        , newTab : Bool
        , externalNotice : String
        , testId : Maybe String
        , shortcut : Maybe { label : String, keys : String }
        }
    | Separator
    | Header String
      -- Content the menu shows but does not offer: the signed-in user at the
      -- top of an account menu, an empty state under a search. Nothing lands
      -- on it.
    | Note (List (Html msg))
      -- A text box above the menu, for a filter whose values come from a
      -- search rather than from a list. It is drawn in the popup but outside
      -- the `role="menu"` list, which may hold nothing but its items (`view`).
      -- The arrow keys stay out of its way.
    | Search { placeholder : String, value : String, onInput : String -> msg }
      -- One row, several choices: light/dark/system, or a filter's AND/OR.
      -- Four rows of radio items say the same thing and cost four times the
      -- menu.
    | Segmented { label : String, options : List (Option msg) }


{-| What choosing an item does: send a message, or go somewhere.
-}
type Action msg
    = Select msg
    | Follow String


type alias Option msg =
    { label : String
    , icon : Maybe Icon
    , selected : Bool
    , onSelect : msg
    }


{-| What a menu item reports about itself. A plain command has nothing to
report; the other two are choices, and the role has to say which kind so a
screen reader announces "selected" rather than nothing — and so the group reads
as one choice rather than several unrelated ones (WCAG 4.1.2).
-}
type State
    = Command
    | Radio Bool
    | Check Bool
    | Choice Bool


init : Model
init =
    Model { open = False, focused = 0, flipped = False }


entryItem : { label : String, action : Action msg, dangerous : Bool, state : State } -> Item msg
entryItem config =
    Item
        { label = config.label
        , icon = Nothing
        , leading = Nothing
        , action = config.action
        , dangerous = config.dangerous
        , state = config.state
        , description = Nothing
        , newTab = False
        , externalNotice = "(opens in a new tab)"
        , testId = Nothing
        , shortcut = Nothing
        }


item : String -> msg -> Item msg
item label onSelect =
    entryItem { label = label, action = Select onSelect, dangerous = False, state = Command }


dangerItem : String -> msg -> Item msg
dangerItem label onSelect =
    entryItem { label = label, action = Select onSelect, dangerous = True, state = Command }


{-| An item that navigates: "Open", "Edit", anything whose result is another
page.

It is a real `a` with an `href` rather than a button that sends a message which
then changes the URL, because navigation is more than the one click. A link can
be opened in a new tab with a middle click or a modifier, its address can be
copied, and a screen reader's list of links finds it. A button that routes
offers none of that, and nobody notices until they reach for it.

To the menu it is an item like any other: `role="menuitem"`, the same roving
tabindex, one stop for the arrow keys, counted by `focusableCount`, and
`withIcon` applies. Enter follows the link, because that is what Enter does on a
link — no handler is involved. Following it closes the menu and puts focus back
on the trigger, so a link opened in a new tab leaves the page as it was, and a
route change moves focus on from there as it would anyway.

Space does not follow it. That is the platform's behavior for a link, and the
APG makes Space on a menu item optional; faking a click from a keypress would
mean a link that behaves unlike every other link on the page.

-}
linkItem : String -> String -> Item msg
linkItem label url =
    entryItem { label = label, action = Follow url, dangerous = False, state = Command }


{-| A link to somewhere destructive — "Delete" when deleting is a confirmation
page rather than a dialog. Everything `linkItem` says applies.
-}
dangerLinkItem : String -> String -> Item msg
dangerLinkItem label url =
    entryItem { label = label, action = Follow url, dangerous = True, state = Command }


{-| One of a set, where choosing one replaces the last. The icon is not the
caller's to pick: on and off have to be the same pair everywhere, or the state
becomes something to work out per menu.
-}
radioItem : Bool -> String -> msg -> Item msg
radioItem checked label onSelect =
    entryItem { label = label, action = Select onSelect, dangerous = False, state = Radio checked }


{-| One of a set, as `radioItem`, where the set is a value to pick — a member's
role — rather than an option to switch: the chosen one carries a check, the
others nothing, so the eye goes to the one that is chosen and not to a column
of empty circles. Still a radio to assistive technology, checked or not.
-}
choiceItem : Bool -> String -> msg -> Item msg
choiceItem chosen label onSelect =
    entryItem { label = label, action = Select onSelect, dangerous = False, state = Choice chosen }


{-| One of a set where any number can be on at once.
-}
checkItem : Bool -> String -> msg -> Item msg
checkItem checked label onSelect =
    entryItem { label = label, action = Select onSelect, dangerous = False, state = Check checked }


separator : Item msg
separator =
    Separator


{-| A label for the group below it. Not focusable and not an item: it names what
the choices are for, which a bare list of options often does not.
-}
header : String -> Item msg
header =
    Header


{-| Something the menu shows rather than offers.
-}
note : List (Html msg) -> Item msg
note =
    Note


searchInput : { placeholder : String, value : String, onInput : String -> msg } -> Item msg
searchInput =
    Search


{-| A row of choices instead of a column of them. Each option is still a
`menuitemradio` and still one stop for the arrow keys — the saving is vertical
space, not keystrokes.
-}
segmented : { label : String, options : List (Option msg) } -> Item msg
segmented =
    Segmented


withIcon : Icon -> Item msg -> Item msg
withIcon icon entry =
    case entry of
        Item config ->
            Item { config | icon = Just icon }

        _ ->
            entry


{-| Something in the item's icon slot that is not an icon: a person's avatar in
a list of people, where the face is what is being picked from.

The slot is a line tall, so what goes in it has to be — an avatar at its
small size is — and as wide as what is in it, with the item's gap after it. Give
every item of a menu the same kind of leading, so the labels line up. An icon wins where an item has both, because an
icon in this slot says what the item does, which is the more useful of the two.

-}
withLeading : Html msg -> Item msg -> Item msg
withLeading leading entry =
    case entry of
        Item config ->
            Item { config | leading = Just leading }

        _ ->
            entry


{-| A sentence under the item's label — what an application in a switcher is
for, say.

It is part of the item's accessible name, inside the same element as the label,
so a screen reader reads "Analytics, reports on usage" as one item rather than
announcing the sentence separately after it. Keep it short: it is read every
time the arrow keys pass the item.

-}
withDescription : String -> Item msg -> Item msg
withDescription description entry =
    case entry of
        Item config ->
            Item { config | description = Just description }

        _ ->
            entry


{-| The keys that do the same from anywhere, shown at the end of the item: `label`
is what is printed ("⌘K", "Ctrl+K"), `keys` the same in the words of
`aria-keyshortcuts` ("Meta+K", "Control+K"), which is what a screen reader says
instead of the printed glyphs.
-}
withShortcut : { label : String, keys : String } -> Item msg -> Item msg
withShortcut shortcut entry =
    case entry of
        Item config ->
            Item { config | shortcut = Just shortcut }

        _ ->
            entry


{-| A link item that opens in a new tab: `target="_blank"` with
`rel="noopener noreferrer"`, an icon, and "(opens in a new tab)" at the end of
its accessible name, because a new tab is a change of context nobody asked for
(WCAG 3.2.5). The words are English until `withExternalNotice`.

Only a `linkItem` or `dangerLinkItem` can open anything; on an item that sends
a message this does nothing.

-}
withNewTab : Item msg -> Item msg
withNewTab entry =
    case entry of
        Item config ->
            Item { config | newTab = True }

        _ ->
            entry


{-| The words `withNewTab` adds, in the language of the page.
-}
withExternalNotice : String -> Item msg -> Item msg
withExternalNotice notice entry =
    case entry of
        Item config ->
            Item { config | externalNotice = notice }

        _ ->
            entry


{-| A `data-cy` attribute on the item's own element — the `button` or `a` the
arrow keys land on — for the end-to-end tests. Does nothing on anything that is
not an item.
-}
withTestId : String -> Item msg -> Item msg
withTestId testId entry =
    case entry of
        Item config ->
            Item { config | testId = Just testId }

        _ ->
            entry


{-| A trigger the caller draws — the signed-in user in the navigation, say,
where the button is a name, a role and a face rather than a word. The label is
still required and still becomes the accessible name.
-}
custom : { label : String, class : String, content : List (Html msg) } -> Trigger msg
custom =
    trigger << Custom


{-| A `data-cy` attribute on the trigger's `button`, as `withTestId` puts one on
an item. The trigger's `id` is `triggerId` of the menu's, for focus; this is the
test hook, kept apart so it can be renamed without breaking a reference.
-}
withTriggerTestId : String -> Trigger msg -> Trigger msg
withTriggerTestId testId (Trigger config) =
    Trigger { config | testId = Just testId }


{-| An icon-only trigger's label in a tooltip, as `Button.withTooltipAt` shows
one: drawn by the stylesheet from the label, visual only, and gone while the
menu is open. For a `custom` trigger drawn as a `ui-button`.
-}
withTriggerTooltipAt : Tooltip.Placement -> Trigger msg -> Trigger msg
withTriggerTooltipAt placement (Trigger config) =
    Trigger { config | tooltip = Just placement }


{-| `labeled`, as a toolbar's button (`Button.quiet`): no border, the text's
own ink, small. Open, it takes the selected tint, as a pressed toggle beside it
does.
-}
quiet : String -> Trigger msg
quiet name =
    custom
        { label = name
        , class = "ui-button ui-button--quiet ui-button--sm"
        , content = [ text name, Icon.toHtml (Icon.solid "fa-chevron-down") ]
        }


{-| Selecting an item closes the menu and returns focus to the trigger. Exposed
as a value rather than a constructor, so the message type stays opaque.
-}
close : Msg
close =
    Closed


{-| Closes the menu and leaves focus alone.

`close` is the one to reach for after an item that acts in place — "Clone",
"Mark as read" — because the item that had focus is gone, and the trigger is
where the keyboard should be again.

This is for an item that **opens a dialog**. The dialog takes focus as it opens,
and moving focus to the trigger afterwards would pull it out from under the
dialog to a button the modal has just made inert. Send this together with
opening the dialog; when the dialog closes, put focus somewhere meaningful
yourself — back on the trigger (`triggerId`), or on the page heading if what the
menu belonged to was deleted.

-}
closeWithoutFocus : Msg
closeWithoutFocus =
    Dismissed


{-| The id of the button that opens the menu with the given id, for putting focus
back on it once a dialog that one of its items opened has closed.
-}
triggerId : String -> String
triggerId menuId =
    menuId ++ "-trigger"


type Msg
    = Toggled
    | Closed
    | Dismissed
    | FocusMoved Int
    | Placed Bool
    | Noop


update : String -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update menuId count msg (Model model) =
    case msg of
        Toggled ->
            if model.open then
                ( Model { model | open = False }, focusTrigger menuId )

            else
                ( Model { open = True, focused = 0, flipped = False }
                , Cmd.batch [ focusOnOpen menuId, measure menuId ]
                )

        Closed ->
            -- Focus returns to the trigger, so the user is never left nowhere.
            ( Model { model | open = False }, focusTrigger menuId )

        Dismissed ->
            -- Clicked elsewhere. Close, but leave focus alone: the user is on
            -- their way to whatever they clicked, and pulling focus back to the
            -- trigger would take it off them.
            ( Model { model | open = False }, Cmd.none )

        FocusMoved index ->
            let
                wrapped =
                    if count == 0 then
                        0

                    else
                        modBy count index
            in
            ( Model { model | focused = wrapped }, focusItem menuId wrapped )

        Placed flipped ->
            ( Model { model | flipped = model.open && flipped }, Cmd.none )

        Noop ->
            ( Model model, Cmd.none )


{-| Where the open list is: if it sticks out of the window on one side only, it
opens towards the other edge of the trigger instead. A filter at the end of a
row that wrapped, a row's actions at the start of a narrow screen — the list
then stays in the window and the page grows no scrollbar. Measured once the
list is drawn, so it may stand out for a frame before it moves.
-}
measure : String -> Cmd Msg
measure menuId =
    Dom.getElement (listId menuId)
        |> Task.map
            (\{ element, viewport } ->
                let
                    pastStart =
                        element.x < viewport.x

                    pastEnd =
                        element.x + element.width > viewport.x + viewport.width
                in
                pastStart /= pastEnd
            )
        |> Task.attempt (Placed << Result.withDefault False)


listId : String -> String
listId menuId =
    menuId ++ "-list"


focusTrigger : String -> Cmd Msg
focusTrigger menuId =
    Task.attempt (always Noop) (Dom.focus (triggerId menuId))


focusItem : String -> Int -> Cmd Msg
focusItem menuId index =
    Task.attempt (always Noop) (Dom.focus (menuId ++ "-item-" ++ String.fromInt index))


{-| The search box if the menu has one, the first item otherwise.

A menu whose values come from a search is a menu you have arrived at in order to
type, and leaving the caret three Shift+Tabs away from where the eye is makes it
keyboard-unreachable in practice. Chained rather than declared, so no caller has
to tell the menu what it contains: focusing an id that is not in the document
fails, and the failure is the fallback.

-}
focusOnOpen : String -> Cmd Msg
focusOnOpen menuId =
    Dom.focus (menuId ++ "-search")
        |> Task.onError (\_ -> Dom.focus (menuId ++ "-item-0"))
        |> Task.attempt (always Noop)


subscriptions : String -> Model -> Sub Msg
subscriptions menuId (Model model) =
    if model.open then
        Sub.batch
            [ Browser.Events.onKeyDown (Decode.map (always Closed) escapeKey)

            -- On release rather than on press, per WCAG 2.5.2: pressing and
            -- then sliding away should cancel, not dismiss. The click still
            -- reaches whatever was pressed, so nothing is swallowed.
            , Browser.Events.onClick (Decode.map (always Dismissed) (outsideMenu menuId))
            ]

    else
        Sub.none


escapeKey : Decode.Decoder ()
escapeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed ()

                else
                    Decode.fail "not escape"
            )


{-| Succeeds only when the event happened outside the menu.

Walks up from the event target looking for the menu's own id. Reaching the top
without finding it means the press was elsewhere, so the menu should close.

-}
outsideMenu : String -> Decode.Decoder ()
outsideMenu menuId =
    Decode.field "target" (isOutside menuId)
        |> Decode.andThen
            (\outside ->
                if outside then
                    Decode.succeed ()

                else
                    Decode.fail "pressed inside the menu"
            )


isOutside : String -> Decode.Decoder Bool
isOutside menuId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if id == menuId then
                        Decode.succeed False

                    else
                        Decode.fail "keep walking up"
                )
        , Decode.lazy (\_ -> Decode.field "parentNode" (isOutside menuId))
        , Decode.succeed True
        ]


toHtml :
    { id : String
    , trigger : Trigger msg
    , alignment : Alignment
    , items : List (Item msg)
    , toMsg : Msg -> msg
    }
    -> Model
    -> Html msg
toHtml config (Model model) =
    div
        (classList
            [ ( "ui-menu", True )
            , ( "is-open", model.open )
            ]
            :: id config.id
            :: (if model.open then
                    [ onFocusLeaving config.id config.toMsg ]

                else
                    []
               )
        )
        [ viewTrigger config (popupKind config.items) model.open
        , if model.open then
            viewPopup config model

          else
            text ""
        ]


{-| What opens under the trigger: a menu, or — for a popup that is only a
search box, a text filter — a small dialog. The trigger says which.
-}
type PopupKind
    = MenuPopup
    | DialogPopup


popupKind : List (Item msg) -> PopupKind
popupKind items =
    if List.all isSearch items then
        DialogPopup

    else
        MenuPopup


isSearch : Item msg -> Bool
isSearch candidate =
    case candidate of
        Search _ ->
            True

        _ ->
            False


{-| The popup holds the search box, if there is one, and the menu under it.

A menu may hold nothing but its items, so a search box is drawn beside the list
rather than in it. The popup is what is positioned, measured and fitted to the
window (`ui-menu__list`); the keys are listened for on it, so the arrow keys
reach the menu from the search box as they did when it was inside. Without a
single item to land on — only a note saying the search is loading, or found
nothing — the list is not a menu but what it shows.

-}
viewPopup :
    { a | id : String, trigger : Trigger msg, alignment : Alignment, items : List (Item msg), toMsg : Msg -> msg }
    -> { b | focused : Int, flipped : Bool }
    -> Html msg
viewPopup config model =
    let
        ( searches, entries ) =
            List.partition isSearch config.items

        alignment =
            case ( config.alignment, model.flipped ) of
                ( Start, True ) ->
                    End

                ( End, True ) ->
                    Start

                _ ->
                    config.alignment

        count =
            focusableCount entries

        isMenu =
            count > 0

        dialogAttributes =
            case popupKind config.items of
                DialogPopup ->
                    [ attribute "role" "dialog", attribute "aria-labelledby" (triggerId config.id) ]

                MenuPopup ->
                    []

        searchBoxes =
            List.map (viewSearch config isMenu) searches

        list =
            if List.isEmpty entries then
                []

            else
                [ ul
                    (class "ui-menu__items"
                        :: (if isMenu then
                                [ id (itemsId config.id)
                                , attribute "role" "menu"
                                , attribute "aria-labelledby" (triggerId config.id)
                                ]

                            else
                                [ attribute "role" "presentation" ]
                           )
                    )
                    (List.indexedMap (viewItem config model.focused) (indexSelectable entries))
                ]
    in
    div
        ([ classList
            [ ( "ui-menu__list", True )
            , ( "ui-menu__list--end", alignment == End )
            , ( "ui-menu__list--side-end", alignment == SideEnd )
            ]
         , id (listId config.id)
         , onKeyNavigation config.toMsg model.focused count
         ]
            ++ dialogAttributes
        )
        (searchBoxes ++ list)


viewSearch : { a | id : String } -> Bool -> Item msg -> Html msg
viewSearch config controlsMenu candidate =
    case candidate of
        Search search ->
            div [ class "ui-menu__search" ]
                [ Html.input
                    ([ id (config.id ++ "-search")
                     , type_ "text"
                     , class "ui-input"
                     , Html.Attributes.placeholder search.placeholder
                     , Html.Attributes.value search.value
                     , attribute "aria-label" search.placeholder
                     , Html.Events.onInput search.onInput
                     ]
                        ++ (if controlsMenu then
                                [ attribute "aria-controls" (itemsId config.id) ]

                            else
                                []
                           )
                    )
                    []
                ]

        _ ->
            text ""


itemsId : String -> String
itemsId menuId =
    menuId ++ "-items"


viewTrigger :
    { a | id : String, trigger : Trigger msg, toMsg : Msg -> msg }
    -> PopupKind
    -> Bool
    -> Html msg
viewTrigger config kind_ open =
    let
        (Trigger { kind, testId, tooltip }) =
            config.trigger

        testIdAttrs =
            TestId.attributes testId

        tooltipAttrs name =
            case tooltip of
                Just placement ->
                    attribute "data-ui-tooltip" name
                        :: (case placement of
                                Tooltip.Above ->
                                    [ attribute "data-ui-tooltip-placement" "above" ]

                                Tooltip.End ->
                                    [ attribute "data-ui-tooltip-placement" "end" ]

                                Tooltip.Start ->
                                    [ attribute "data-ui-tooltip-placement" "start" ]

                                _ ->
                                    []
                           )

                Nothing ->
                    []

        shared name extraClass content =
            button
                ([ id (triggerId config.id)
                 , type_ "button"
                 , class extraClass
                 , attribute "aria-haspopup"
                    (case kind_ of
                        MenuPopup ->
                            "menu"

                        DialogPopup ->
                            "dialog"
                    )
                 , attribute "aria-label" name
                 , attribute "aria-expanded"
                    (if open then
                        "true"

                     else
                        "false"
                    )
                 , onClick (config.toMsg Toggled)
                 ]
                    ++ testIdAttrs
                    ++ tooltipAttrs name
                )
                content
    in
    case kind of
        Labeled name ->
            shared name
                "ui-button ui-button--secondary"
                [ text name
                , Icon.toHtml (Icon.solid "fa-chevron-down")
                ]

        Kebab name ->
            -- No tooltip. The aria-label is the accessible name, so nothing is
            -- lost, and a kebab repeats on every row of a list — a tooltip on
            -- each one is noise rather than help. A caller that wants one can
            -- wrap the menu in Ui.Tooltip.
            shared name
                "ui-menu__kebab"
                [ Icon.toHtml (Icon.solid "fa-ellipsis-vertical") ]

        Split name ->
            shared name
                "ui-button ui-button--secondary ui-button--icon"
                [ Icon.toHtml (Icon.solid "fa-chevron-down") ]

        Custom drawn ->
            shared drawn.label drawn.class drawn.content


{-| How many things in this entry the arrow keys can land on. Everything that is
not a target is zero, and a row of choices is one per choice.
-}
focusCount : Item msg -> Int
focusCount candidate =
    case candidate of
        Item _ ->
            1

        Segmented { options } ->
            List.length options

        _ ->
            0


{-| The number of stops a set of items adds up to. The caller passes it to
`update`, which needs it to wrap around — and getting it wrong leaves a dead
index the arrow keys stop on.
-}
focusableCount : List (Item msg) -> Int
focusableCount =
    List.foldl (\candidate total -> total + focusCount candidate) 0


{-| Pairs each entry with its position among the selectable ones, so separators
and headers do not consume an index the arrow keys would then land on.
-}
indexSelectable : List (Item msg) -> List ( Maybe Int, Item msg )
indexSelectable items =
    List.foldl
        (\candidate ( position, acc ) ->
            if focusCount candidate > 0 then
                ( position + focusCount candidate, acc ++ [ ( Just position, candidate ) ] )

            else
                ( position, acc ++ [ ( Nothing, candidate ) ] )
        )
        ( 0, [] )
        items
        |> Tuple.second


viewItem :
    { a | id : String, toMsg : Msg -> msg }
    -> Int
    -> Int
    -> ( Maybe Int, Item msg )
    -> Html msg
viewItem config focused _ ( position, candidate ) =
    case ( candidate, position ) of
        ( Item entry, Just index ) ->
            let
                common =
                    [ id (config.id ++ "-item-" ++ String.fromInt index)
                    , attribute "role" (itemRole entry.state)

                    -- Roving tabindex: exactly one item is reachable by Tab.
                    , tabindex
                        (if index == focused then
                            0

                         else
                            -1
                        )
                    , classList
                        [ ( "ui-menu__item", True )
                        , ( "ui-menu__item--danger", entry.dangerous )
                        ]
                    ]
                        ++ TestId.attributes entry.testId
                        ++ (case entry.shortcut of
                                Just shortcut ->
                                    [ attribute "aria-keyshortcuts" shortcut.keys ]

                                Nothing ->
                                    []
                           )

                opensNewTab =
                    case entry.action of
                        Follow _ ->
                            entry.newTab

                        Select _ ->
                            False

                label =
                    case entry.description of
                        Just description ->
                            -- One element, so the description is part of the
                            -- name rather than a second announcement.
                            span [ class "ui-menu__item-label" ]
                                [ span [ class "ui-menu__item-text" ] [ text entry.label ]
                                , span [ class "ui-menu__item-description" ] [ text description ]
                                ]

                        Nothing ->
                            text entry.label

                external =
                    if opensNewTab then
                        [ span [ class "ui-menu__item-external" ]
                            [ Icon.toHtml (Icon.solid "fa-arrow-up-right-from-square")
                            , span [ class "ui-visually-hidden" ] [ text (" " ++ entry.externalNotice) ]
                            ]
                        ]

                    else
                        []

                shortcutHint =
                    case entry.shortcut of
                        -- Hidden from the name: `aria-keyshortcuts` says it.
                        Just shortcut ->
                            [ Html.kbd [ class "ui-menu__item-shortcut", attribute "aria-hidden" "true" ] [ text shortcut.label ] ]

                        Nothing ->
                            []

                content =
                    (case ( stateIcon entry.state, entry.icon, entry.leading ) of
                        ( Just icon, _, _ ) ->
                            [ span [ class "ui-menu__item-icon" ] [ Icon.toHtml icon ], label ]

                        ( Nothing, Just icon, _ ) ->
                            [ span [ class "ui-menu__item-icon" ] [ Icon.toHtml icon ], label ]

                        ( Nothing, Nothing, Just leading ) ->
                            [ span [ class "ui-menu__item-icon ui-menu__item-icon--leading" ] [ leading ], label ]

                        ( Nothing, Nothing, Nothing ) ->
                            if entry.state == Choice False then
                                [ span [ class "ui-menu__item-icon", attribute "aria-hidden" "true" ] [], label ]

                            else
                                [ label ]
                    )
                        ++ external
                        ++ shortcutHint
            in
            li [ attribute "role" "none" ]
                [ case entry.action of
                    Select onSelect ->
                        button
                            (common
                                ++ type_ "button"
                                :: onClick onSelect
                                :: checkedAttribute entry.state
                            )
                            content

                    Follow url ->
                        -- The click is not prevented, so the browser (or the
                        -- application's own link handling) still navigates;
                        -- the menu only closes behind it.
                        Html.a
                            (common
                                ++ [ href url
                                   , onClick (config.toMsg Closed)
                                   ]
                                ++ (if opensNewTab then
                                        [ Html.Attributes.target "_blank", Html.Attributes.rel "noopener noreferrer" ]

                                    else
                                        []
                                   )
                            )
                            content
                ]

        ( Segmented group, Just start ) ->
            li [ attribute "role" "presentation", class "ui-menu__segmented" ]
                [ span [ class "ui-menu__segmented-label" ] [ text group.label ]
                , div
                    [ class "ui-menu__segmented-options"
                    , attribute "role" "group"
                    , attribute "aria-label" group.label
                    ]
                    (List.indexedMap (viewOption config focused start) group.options)
                ]

        ( Header label, _ ) ->
            -- Presentational: it names the group without being something the
            -- arrow keys stop on.
            li [ attribute "role" "presentation", class "ui-menu__header" ] [ text label ]

        ( Note content, _ ) ->
            li [ attribute "role" "presentation", class "ui-menu__note" ] content

        ( Search _, _ ) ->
            -- Drawn above the list, outside the menu (`viewSearch`).
            text ""

        _ ->
            li
                [ attribute "role" "separator"
                , class "ui-menu__separator"
                ]
                []


{-| One choice in a segmented row. A `menuitemradio` like any other radio item,
so the arrow keys and the announcement are the same — only the layout differs.
-}
viewOption : { a | id : String } -> Int -> Int -> Int -> Option msg -> Html msg
viewOption config focused start offset option =
    let
        index =
            start + offset
    in
    button
        [ id (config.id ++ "-item-" ++ String.fromInt index)
        , type_ "button"
        , attribute "role" "menuitemradio"
        , attribute "aria-checked" (boolToString option.selected)
        , attribute "aria-label" option.label
        , tabindex
            (if index == focused then
                0

             else
                -1
            )
        , classList
            [ ( "ui-menu__option", True )
            , ( "is-selected", option.selected )
            ]
        , onClick option.onSelect
        ]
        (case option.icon of
            Just icon ->
                [ Icon.toHtml icon ]

            Nothing ->
                [ text option.label ]
        )


{-| The role the item reports, and the glyph that says the same thing to
everyone else. Checked and unchecked are a matched pair — a filled circle
against an empty one — because a check that only appears when on leaves the off
state looking like a missing icon.
-}
itemRole : State -> String
itemRole state =
    case state of
        Command ->
            "menuitem"

        Radio _ ->
            "menuitemradio"

        Choice _ ->
            "menuitemradio"

        Check _ ->
            "menuitemcheckbox"


checkedAttribute : State -> List (Html.Attribute msg)
checkedAttribute state =
    case state of
        Command ->
            []

        Radio checked ->
            [ attribute "aria-checked" (boolToString checked) ]

        Check checked ->
            [ attribute "aria-checked" (boolToString checked) ]

        Choice checked ->
            [ attribute "aria-checked" (boolToString checked) ]


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"


stateIcon : State -> Maybe Icon
stateIcon state =
    case state of
        Command ->
            Nothing

        Radio True ->
            Just (Icon.solid "fa-circle-check")

        Radio False ->
            Just (Icon.regular "fa-circle")

        Check True ->
            Just (Icon.solid "fa-square-check")

        Check False ->
            Just (Icon.regular "fa-square")

        Choice True ->
            Just (Icon.solid "fa-check")

        Choice False ->
            Nothing


{-| Closes when focus leaves the menu altogether.

Tab moved focus out but left the menu standing: the arrow keys were handled and
Tab was not, so the list stayed open behind whatever the user had moved on to.

Handled on `focusout` rather than by intercepting Tab. Closing on the keypress
would remove the list from the document before the browser had finished moving
focus through it, and focus would land on nothing. This lets the move happen and
closes afterwards, which also covers Shift+Tab and focus taken away by anything
else.

Except an item that falls to nothing, which is the item being redrawn rather
than left: a choice that changes the list's shape — a filter's second value
brings in the OR/AND choice above it — has the item chosen replaced under the
pointer. Focus is put back at the same place in the list, which stays open. A
click elsewhere still closes it (`subscriptions`).

-}
onFocusLeaving : String -> (Msg -> msg) -> Html.Attribute msg
onFocusLeaving menuId toMsg =
    on "focusout"
        (Decode.field "relatedTarget" (Decode.nullable (Decode.succeed ()))
            |> Decode.andThen
                (\related ->
                    case related of
                        Nothing ->
                            Decode.oneOf
                                [ Decode.at [ "target", "id" ] Decode.string
                                    |> Decode.andThen (redrawnItem menuId)
                                    |> Decode.map (toMsg << FocusMoved)
                                , Decode.succeed (toMsg Dismissed)
                                ]

                        Just () ->
                            Decode.field "relatedTarget" (isOutside menuId)
                                |> Decode.andThen
                                    (\outside ->
                                        if outside then
                                            Decode.succeed (toMsg Dismissed)

                                        else
                                            Decode.fail "focus is still inside the menu"
                                    )
                )
        )


redrawnItem : String -> String -> Decode.Decoder Int
redrawnItem menuId elementId =
    let
        prefix =
            menuId ++ "-item-"
    in
    if String.startsWith prefix elementId then
        String.dropLeft (String.length prefix) elementId
            |> String.toInt
            |> Maybe.map Decode.succeed
            |> Maybe.withDefault (Decode.fail "not an item")

    else
        Decode.fail "not an item"


onKeyNavigation : (Msg -> msg) -> Int -> Int -> Html.Attribute msg
onKeyNavigation toMsg focused count =
    preventDefaultOn "keydown"
        (Decode.map2 Tuple.pair typedInto (Decode.field "key" Decode.string)
            |> Decode.andThen
                (\( typing, key ) ->
                    case ( key, typing ) of
                        -- From a search box the first Down goes *to* the
                        -- current item rather than past it: focus is in the
                        -- text field, so nothing has been landed on yet.
                        ( "ArrowDown", True ) ->
                            Decode.succeed ( toMsg (FocusMoved focused), True )

                        ( "ArrowUp", True ) ->
                            Decode.succeed ( toMsg (FocusMoved (count - 1)), True )

                        -- Home and End belong to the caret while there is one.
                        ( "Home", True ) ->
                            Decode.fail "the caret's key"

                        ( "End", True ) ->
                            Decode.fail "the caret's key"

                        ( "ArrowDown", False ) ->
                            Decode.succeed ( toMsg (FocusMoved (focused + 1)), True )

                        ( "ArrowUp", False ) ->
                            Decode.succeed ( toMsg (FocusMoved (focused - 1 + count)), True )

                        ( "Home", False ) ->
                            Decode.succeed ( toMsg (FocusMoved 0), True )

                        ( "End", False ) ->
                            Decode.succeed ( toMsg (FocusMoved (count - 1)), True )

                        _ ->
                            Decode.fail "not a navigation key"
                )
        )


typedInto : Decode.Decoder Bool
typedInto =
    Decode.oneOf
        [ Decode.at [ "target", "tagName" ] Decode.string
            |> Decode.map (\tag -> tag == "INPUT" || tag == "TEXTAREA")
        , Decode.succeed False
        ]
