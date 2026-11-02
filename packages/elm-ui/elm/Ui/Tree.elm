module Ui.Tree exposing
    ( Node
    , Tone(..)
    , Words
    , node
    , pills
    , toHtml
    , withAction
    , withChildren
    , withCurrent
    , withDisabled
    , withEnd
    , withHighlight
    , withIcon
    , withPlaceholderLabel
    , withRoot
    , withTestId
    , withTone
    )

{-| A navigation tree: places to go, some of them with places inside them.

The questionnaire's chapters and the questions and items inside the open one are
its first user; the document template editor's files are the next.

**Not the ARIA `tree`.** `role="tree"` promises a single tab stop, arrow keys that
walk and open the branches, typeahead and selection — a widget, announced as one,
with a keyboard model to learn before it can be used. What is here is a
navigation: every entry is a button that goes somewhere, every branch that can
open is a disclosure button beside it, and the nesting is nested lists, which a
screen reader already counts and announces ("list, 4 items, level 2"). Tab walks
it like any other run of controls, which is what a list of places needs; the
arrow keys stay with the page, where they scroll.

**A branch is a disclosure.** The toggle is a real button with `aria-expanded`,
named after what it opens ("Show what is in Chapter 2"), and before the entry it
belongs to, where the eye expects a caret. A closed branch renders no children at
all: a tree folds away most of what it holds, and the folded part is the part not
worth building.

**The current place** is `aria-current`, and a tint and a weight, never color
alone. Entries are buttons rather than links because what they do is move within
the page the reader is on — a chapter, a question — and the caller says what.

**Little to build an entry.** A questionnaire's open chapter can list hundreds
of questions and items, all rebuilt when the chapter changes, so an entry is its
`li`, its button and its text, with an icon and a toggle only where it has them,
and as few attributes as that can carry: the `li` has no class (the tree's
stylesheet reaches it through the list), and the entry has no `type`, since a
navigation is never inside a form. The `li` is a grid whose first column is the
toggle's: an entry without one sits in the second column anyway, and its
children's list below it, so the labels line up down a level with no spacer and no
row wrapper.

-}

import Html exposing (Html, button, i, li, text)
import Html.Attributes exposing (attribute, class, disabled, type_)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Maybe.Extra as Maybe
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId


type Node msg
    = Node
        { key : String
        , label : String
        , onSelect : msg
        , icon : Maybe Icon
        , current : Bool
        , placeholder : Bool
        , action : Bool
        , highlight : Bool
        , root : Bool
        , disabled : Bool
        , tone : Maybe Tone
        , end : List (Html msg)
        , branch : Branch msg
        , testId : Maybe String
        }


{-| What has happened to the thing an entry stands for, where a tree shows
changes as well as places: something new, something altered, something on its
way out. It is a second signal beside a word at the end of the entry, never the
only one — color alone fails 1.4.1.
-}
type Tone
    = Success
    | Info
    | Danger


type Branch msg
    = Leaf
    | Folded msg
    | Unfolded msg (List (Node msg))


{-| An entry: a `key` unique among its siblings (the list is keyed, so an entry
keeps its element as the ones around it come and go), what it says, and what
choosing it does.
-}
node : { key : String, label : String, onSelect : msg } -> Node msg
node config =
    Node
        { key = config.key
        , label = config.label
        , onSelect = config.onSelect
        , icon = Nothing
        , current = False
        , placeholder = False
        , action = False
        , highlight = False
        , root = False
        , disabled = False
        , tone = Nothing
        , end = []
        , branch = Leaf
        , testId = Nothing
        }


{-| A branch. `expanded` says whether it is open and `onToggle` opens or closes
it; `children` are rendered only while it is open, so a caller can pass them
unbuilt when it is not.
-}
withChildren : { expanded : Bool, onToggle : msg } -> List (Node msg) -> Node msg -> Node msg
withChildren config children (Node entry) =
    Node
        { entry
            | branch =
                if config.expanded then
                    Unfolded config.onToggle children

                else
                    Folded config.onToggle
        }


withIcon : Icon -> Node msg -> Node msg
withIcon icon (Node entry) =
    Node { entry | icon = Just icon }


{-| The place the reader is in.
-}
withCurrent : Bool -> Node msg -> Node msg
withCurrent current (Node entry) =
    Node { entry | current = current }


{-| The label is a stand-in rather than a name ("Item 3", for an item nobody has
named yet), and is set in italics so it reads as one.
-}
withPlaceholderLabel : Node msg -> Node msg
withPlaceholderLabel (Node entry) =
    Node { entry | placeholder = True }


{-| What goes at the end of the entry, inside it: a count, a done mark. It is part
of the entry's name, so it has to say what it is in words (a `Ui.Badge` with a
description, an icon with visually hidden text). It is always shown: a label too
long for the line is what gives way.
-}
withEnd : List (Html msg) -> Node msg -> Node msg
withEnd end (Node entry) =
    Node { entry | end = end }


{-| An entry that makes something rather than goes somewhere — "Add chapter" at
the end of a collection. It is set apart from its siblings and slightly further
in, so the eye can tell at a glance which rows are the collection and which is
the way to add to it, without it becoming a different kind of control.
-}
withAction : Node msg -> Node msg
withAction (Node entry) =
    Node { entry | action = True }


{-| The entry the tree is about, which is neither where the reader is nor
somewhere they can go: the entity a move dialog is moving, shown in its present
place among the places it could go to. It is picked out with a ground of its
own, so it can be found at a glance in a tree where everything else is either a
target or out of reach.

Say what it is in words as well (`withEnd`), since a ground is a color.

-}
withHighlight : Node msg -> Node msg
withHighlight (Node entry) =
    Node { entry | highlight = True }


{-| The thing every other entry is inside — the knowledge model a tree of its
chapters belongs to. It takes the whole width, including the column the toggles
are in, so what follows reads as its contents rather than as its equals. It has
no toggle of its own: the root of a tree is not something to fold away.
-}
withRoot : Node msg -> Node msg
withRoot (Node entry) =
    Node { entry | root = True }


{-| An entry that names something no longer worth opening — an entity deleted
but not yet published. The button stays in the tree, says what happened at its
end and is out of reach: nothing is behind it to go to.
-}
withDisabled : Bool -> Node msg -> Node msg
withDisabled value (Node entry) =
    Node { entry | disabled = value }


{-| What has happened to it, as a color on the label. Say it in words too — at
the end of the entry, where `withEnd` puts them.
-}
withTone : Tone -> Node msg -> Node msg
withTone tone (Node entry) =
    Node { entry | tone = Just tone }


withTestId : String -> Node msg -> Node msg
withTestId testId (Node entry) =
    Node { entry | testId = Just testId }


{-| What the toggles say: `expand label` on a closed branch, `collapse label` on
an open one ("Show what is in Chapter 2", "Hide what is in Chapter 2").
-}
type alias Words =
    { expand : String -> String
    , collapse : String -> String
    }


{-| The tree. `label` names the top list; the landmark round it (a `nav`, with a
name of its own) is the caller's, since a tree is one part of a navigation more
often than the whole of it.
-}
toHtml : { label : String, words : Words } -> List (Node msg) -> Html msg
toHtml config nodes =
    Keyed.ul [ class "ui-tree", attribute "aria-label" config.label ]
        (List.map (viewNode config.words) nodes)


{-| `toHtml`, with the top level as pills: each entry on a light ground of its
own, apart from the next, with its toggle inside it — for a short list of large
places (a questionnaire's chapters) whose boundaries have to be seen at a glance.
What is inside an open one is the plain tree.
-}
pills : { label : String, words : Words } -> List (Node msg) -> Html msg
pills config nodes =
    Keyed.ul [ class "ui-tree ui-tree--pills", attribute "aria-label" config.label ]
        (List.map (viewNode config.words) nodes)


viewNode : Words -> Node msg -> ( String, Html msg )
viewNode words (Node entry) =
    let
        toggle onToggle expanded =
            button
                [ type_ "button"
                , class "ui-tree__toggle"
                , attribute "aria-expanded" (boolString expanded)
                , attribute "aria-label"
                    (if expanded then
                        words.collapse entry.label

                     else
                        words.expand entry.label
                    )
                , onClick onToggle
                ]
                [ Icon.toHtml
                    (if expanded then
                        Icon.solid "fa-chevron-down"

                     else
                        Icon.solid "fa-chevron-right"
                    )
                ]

        ( before, after ) =
            case entry.branch of
                Leaf ->
                    ( [], [] )

                Folded onToggle ->
                    ( [ toggle onToggle False ], [] )

                Unfolded onToggle nested ->
                    ( [ toggle onToggle True ]
                    , if List.isEmpty nested then
                        []

                      else
                        [ Keyed.ul [ class "ui-tree__group" ] (List.map (viewNode words) nested) ]
                    )

        currentAttrs =
            if entry.current then
                [ attribute "aria-current" "true" ]

            else
                []

        testIdAttrs =
            TestId.attributes entry.testId

        -- The icon is the `i` itself, with the tree's class beside the
        -- glyph's, rather than a wrapper round `Icon.toHtml`.
        content =
            case entry.icon of
                Just icon ->
                    i [ class ("ui-icon ui-tree__icon " ++ Icon.className icon), attribute "aria-hidden" "true" ] []
                        :: text entry.label
                        :: entry.end

                Nothing ->
                    text entry.label :: entry.end

        classes =
            String.join " "
                (Maybe.values
                    [ Just "ui-tree__entry"
                    , justIf entry.placeholder "is-placeholder"
                    , justIf entry.action "is-action"
                    , justIf entry.root "is-root"
                    , justIf entry.highlight "is-highlighted"
                    , justIf (not (List.isEmpty entry.end)) "has-end"
                    , Maybe.map toneClass entry.tone
                    ]
                )
    in
    ( entry.key
    , li []
        (before
            ++ button
                (class classes
                    :: onClick entry.onSelect
                    :: disabled entry.disabled
                    :: currentAttrs
                    ++ testIdAttrs
                )
                content
            :: after
        )
    )


justIf : Bool -> String -> Maybe String
justIf condition value =
    if condition then
        Just value

    else
        Nothing


toneClass : Tone -> String
toneClass tone =
    case tone of
        Success ->
            "is-success"

        Info ->
            "is-info"

        Danger ->
            "is-danger"


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"
