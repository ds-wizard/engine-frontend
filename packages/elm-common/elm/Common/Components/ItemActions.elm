module Common.Components.ItemActions exposing
    ( Action
    , action
    , buttons
    , choice
    , dangerAction
    , focusableCount
    , groups
    , labeledMenu
    , link
    , menu
    , note
    , splitMenu
    , toMenuItems
    , withTestId
    )

{-| The actions of one thing — a row in a listing, the header of a detail page —
as a kebab menu.

An action is one of three things, and each renders as what it is:

  - `link` — "Open", "Edit", anything whose result is another page. A real `a`
    through `Ui.Menu.linkItem`, so middle click, "copy link address" and a
    screen reader's list of links all work on it.
  - `action` — something that happens here: "Clone", "Export".
  - `dangerAction` — something destructive: "Delete". Marked by color **and**
    by the menu's danger styling of the label and icon, and always behind a
    confirmation the caller owns.

Pages decide which actions a user may see with `groups`, which takes the
visibility next to each action, drops the hidden ones, and puts a separator
between groups that still have something in them — so a menu never starts or
ends with a separator, or shows two in a row, however the permissions fall.

`withTestId` puts a `data-cy` on the item's own element, through
`Ui.Menu.withTestId`.

-}

import Html exposing (Html, text)
import List.Extra as List
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon exposing (Icon)
import Ui.Menu as Menu


type Action msg
    = Link { icon : Icon, label : String, url : String }
    | Command { icon : Icon, label : String, onSelect : msg }
    | Danger { icon : Icon, label : String, onSelect : msg }
    | Choice { label : String, chosen : Bool, onSelect : msg }
    | Note (List (Html msg))
    | Separator
    | TestId String (Action msg)


link : { icon : Icon, label : String, url : String } -> Action msg
link =
    Link


{-| One of a set of values the thing can have, the one it has checked: a
member's role. Choosing one sets it (`Ui.Menu.choiceItem`).
-}
choice : { label : String, chosen : Bool, onSelect : msg } -> Action msg
choice =
    Choice


action : { icon : Icon, label : String, onSelect : msg } -> Action msg
action =
    Command


dangerAction : { icon : Icon, label : String, onSelect : msg } -> Action msg
dangerAction =
    Danger


{-| Something the menu says rather than offers — how a project is shared,
under the actions that change it (`Ui.Menu.note`).

It is not a stop for the arrow keys and it is dropped by `buttons`, which has
nowhere to put it. Keep it to a sentence, and put it in a group of its own so
that `groups` separates it from the actions above.

-}
note : List (Html msg) -> Action msg
note =
    Note


{-| A `data-cy` attribute on the menu item this action becomes — the `a` or the
`button` — for the end-to-end tests. Does nothing on a separator.
-}
withTestId : String -> Action msg -> Action msg
withTestId testId candidate =
    case candidate of
        Separator ->
            Separator

        Note content ->
            Note content

        TestId _ inner ->
            TestId testId inner

        _ ->
            TestId testId candidate


{-| Groups of actions, each with whether it is visible.

Hidden actions are dropped, empty groups are dropped, and a separator goes
between the groups that are left.

-}
groups : List (List ( Action msg, Bool )) -> List (Action msg)
groups actionGroups =
    actionGroups
        |> List.map (List.filterMap visibleAction)
        |> List.filterNot List.isEmpty
        |> List.intersperse [ Separator ]
        |> List.concat


visibleAction : ( Action msg, Bool ) -> Maybe (Action msg)
visibleAction ( candidate, visible ) =
    if visible then
        Just candidate

    else
        Nothing


{-| The same actions as icon-only buttons, for a table's actions column
(`Ui.Table.actions`, which adds the tooltips): a link is an icon-only link, an
action an icon-only button, a destructive one a danger icon-only button;
separators are dropped.

`subject` names the thing the row is — "Horizon Europe DMP" — and each button's
accessible name is its label followed by it ("Delete Horizon Europe DMP"), so a
column of identical "Delete"s is still tell-apart-able to a screen reader.

-}
buttons : String -> List (Action msg) -> List (Button msg)
buttons subject =
    List.filterMap (toButton subject)


toButton : String -> Action msg -> Maybe (Button msg)
toButton subject candidate =
    let
        named config button =
            Button.withAccessibleName (config.label ++ " " ++ subject) button
    in
    case candidate of
        Link config ->
            Just (named config (Button.iconOnlyLink config.icon config.label config.url))

        Command config ->
            Just (named config (Button.iconOnly config.icon config.label config.onSelect))

        Danger config ->
            Just (named config (Button.dangerIconOnly config.icon config.label config.onSelect))

        Note _ ->
            Nothing

        Separator ->
            Nothing

        -- A value to pick is a menu's, with the others beside it; alone as a
        -- button it would not say what it sets.
        Choice _ ->
            Nothing

        TestId testId inner ->
            Maybe.map (Button.withTestId testId) (toButton subject inner)


toMenuItems : List (Action msg) -> List (Menu.Item msg)
toMenuItems =
    List.map toMenuItem


toMenuItem : Action msg -> Menu.Item msg
toMenuItem candidate =
    case candidate of
        Link config ->
            Menu.withIcon config.icon (Menu.linkItem config.label config.url)

        Command config ->
            Menu.withIcon config.icon (Menu.item config.label config.onSelect)

        Danger config ->
            Menu.withIcon config.icon (Menu.dangerItem config.label config.onSelect)

        Choice config ->
            Menu.choiceItem config.chosen config.label config.onSelect

        Note content ->
            Menu.note content

        Separator ->
            Menu.separator

        TestId testId inner ->
            Menu.withTestId testId (toMenuItem inner)


{-| The number of stops the arrow keys have in these actions, for
`Ui.Menu.update`. Separators are not stops.
-}
focusableCount : List (Action msg) -> Int
focusableCount =
    Menu.focusableCount << toMenuItems


{-| The kebab menu. End-aligned, because actions sit at the end of a row and a
start-aligned menu there runs off the page.

The label is the trigger's accessible name and should say whose actions these
are — "Actions for Horizon Europe DMP" — because a list of rows each with a
button called "Actions" is a list a screen reader user cannot tell apart.

Nothing is rendered when there are no actions: a menu that opens onto nothing is
a tab stop that lies.

-}
menu :
    { id : String
    , label : String
    , actions : List (Action msg)
    , model : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
menu config =
    viewMenu (Menu.kebab config.label) config


{-| The same menu behind a button that shows what the menu sets — a member's
role, with the other roles and the way to remove them inside. `content` is what
the button shows; `label` is its accessible name, which has to start with what
it shows (WCAG 2.5.3) and say what it is about: "User, role of Ada Lovelace".
-}
labeledMenu :
    { id : String
    , label : String
    , content : List (Html msg)
    , actions : List (Action msg)
    , model : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
labeledMenu config =
    viewMenu
        (Menu.custom
            { label = config.label
            , class = "ui-button ui-button--secondary ui-button--sm"
            , content = config.content ++ [ Icon.toHtml (Icon.solid "fa-chevron-down") ]
            }
        )
        config


{-| The same menu behind `Ui.Menu.split`, the chevron half of a split button —
for actions that are more of the button they are joined to, not the thing's
overflow. The label names what they are: "Sharing options".
-}
splitMenu :
    { id : String
    , label : String
    , actions : List (Action msg)
    , model : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
splitMenu config =
    viewMenu (Menu.split config.label) config


viewMenu :
    Menu.Trigger msg
    -> { a | id : String, actions : List (Action msg), model : Menu.Model, toMsg : Menu.Msg -> msg }
    -> Html msg
viewMenu trigger config =
    if List.isEmpty config.actions then
        text ""

    else
        Menu.toHtml
            { id = config.id
            , trigger = trigger
            , alignment = Menu.End
            , items = toMenuItems config.actions
            , toMsg = config.toMsg
            }
            config.model
