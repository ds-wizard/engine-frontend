module Common.Pages.DocumentTemplateEditors.Editor.Groups exposing
    ( Groups
    , Side(..)
    , activate
    , activeSide
    , close
    , currentOf
    , init
    , isSplit
    , keepOnly
    , moveToOtherSide
    , open
    , openEntries
    , tabsOf
    )

{-| What is open in the middle of the Files tab.

**A file that is opened stays open.** The tree opens files into a strip of tabs
rather than replacing what is there, because a template is read across its
files: a layout beside the partial it includes, a macro beside its use. Closing
is a decision of its own, which is what the close button on each tab is for.

**Two sides, and only ever two.** An open file can be moved to the other side,
which is what puts two editors beside each other; moving the last one back takes
the split away again. There is no third side and no free arrangement of panes —
the wizard has two, the reason to have two is comparing one file with another,
and every further pane is a layout to manage rather than a file to read.

**A side is never left empty while the other has something.** Closing the last
tab of the first side does not leave a hole where an editor was: the second side
moves into it. That is what makes "move to the other side"
reversible.

The entries are `Files.Entry`, which names a file or an asset by its uuid, so a
rename leaves the tabs alone: it is the same file under another name. A delete
does not — `keepOnly` takes the tabs of everything that is gone.

-}

import Common.Pages.DocumentTemplateEditors.Editor.Files as Files exposing (Entry)
import List.Extra as List
import Set exposing (Set)


type Side
    = First
    | Second


type Groups
    = Groups
        { first : Group
        , second : Group
        , active : Side
        }


type alias Group =
    { tabs : List Entry
    , current : Maybe Entry
    }


init : Groups
init =
    Groups
        { first = { tabs = [], current = Nothing }
        , second = { tabs = [], current = Nothing }
        , active = First
        }


{-| Opening from the file list. A file that is already open somewhere is
brought forward where it is, rather than opened a second time on the other
side; anything else joins the side that was last worked in.
-}
open : Entry -> Groups -> Groups
open entry (Groups groups) =
    if List.member entry groups.first.tabs then
        Groups { groups | first = show entry groups.first, active = First }

    else if List.member entry groups.second.tabs then
        Groups { groups | second = show entry groups.second, active = Second }

    else
        set groups.active (add entry (groupOf groups.active (Groups groups))) (Groups groups)


{-| Bringing forward a tab that is already open, on the side it is open on.
-}
activate : Side -> Entry -> Groups -> Groups
activate side entry (Groups groups) =
    set side (show entry (groupOf side (Groups groups))) (Groups { groups | active = side })


{-| Closing one tab. What was beside it takes its place; a side left with
nothing gives its room back.
-}
close : Side -> Entry -> Groups -> Groups
close side entry groups =
    let
        group =
            groupOf side groups

        tabs =
            List.filter ((/=) entry) group.tabs
    in
    consolidate
        (set side
            { tabs = tabs
            , current =
                if group.current == Just entry then
                    List.last tabs

                else
                    group.current
            }
            groups
        )


{-| The tabs of everything that still exists. Deleting a file, or a folder with
files in it, leaves tabs open on files the server no longer has.
-}
keepOnly : Set String -> Groups -> Groups
keepOnly uuids groups =
    let
        kept group =
            let
                tabs =
                    List.filter (\entry -> Set.member (uuidOf entry) uuids) group.tabs
            in
            { tabs = tabs
            , current =
                case group.current of
                    Just current ->
                        if List.member current tabs then
                            Just current

                        else
                            List.last tabs

                    Nothing ->
                        Nothing
            }
    in
    consolidate (mapSides kept groups)


{-| The one action that makes and unmakes the split: the tab shown on `side`
goes to the other side, and the side it went to is the one being worked on.

The side is given rather than taken from which one is active, because the button
that sends this is in that side's own strip: a control acts on what it is part
of, not on whatever was touched last.

-}
moveToOtherSide : Side -> Groups -> Groups
moveToOtherSide side groups =
    let
        from =
            groupOf side groups
    in
    case from.current of
        Just entry ->
            let
                other =
                    otherSide side

                remaining =
                    List.filter ((/=) entry) from.tabs
            in
            -- The tab is taken off one side and put on the other, and only then
            -- are the sides consolidated. Taking it off with `close` would
            -- consolidate in the middle of the move: the last file of the first
            -- side would leave that side empty, the second would be promoted
            -- into it, and the file would land on a "second side" that is now
            -- the one it came from — the two swapping places instead of
            -- becoming one.
            groups
                |> set side { tabs = remaining, current = List.last remaining }
                |> (\moved -> set other (add entry (groupOf other moved)) moved)
                |> setActive other
                |> consolidate

        Nothing ->
            groups


setActive : Side -> Groups -> Groups
setActive side (Groups groups) =
    Groups { groups | active = side }


isSplit : Groups -> Bool
isSplit (Groups groups) =
    not (List.isEmpty groups.second.tabs)


activeSide : Groups -> Side
activeSide (Groups groups) =
    groups.active


tabsOf : Side -> Groups -> List Entry
tabsOf side groups =
    (groupOf side groups).tabs


currentOf : Side -> Groups -> Maybe Entry
currentOf side groups =
    (groupOf side groups).current


{-| Everything open on either side, which is what has to be fetched and kept.
-}
openEntries : Groups -> List Entry
openEntries (Groups groups) =
    groups.first.tabs ++ groups.second.tabs



-- THE PARTS


groupOf : Side -> Groups -> Group
groupOf side (Groups groups) =
    case side of
        First ->
            groups.first

        Second ->
            groups.second


set : Side -> Group -> Groups -> Groups
set side group (Groups groups) =
    case side of
        First ->
            Groups { groups | first = group }

        Second ->
            Groups { groups | second = group }


mapSides : (Group -> Group) -> Groups -> Groups
mapSides f (Groups groups) =
    Groups { groups | first = f groups.first, second = f groups.second }


otherSide : Side -> Side
otherSide side =
    case side of
        First ->
            Second

        Second ->
            First


add : Entry -> Group -> Group
add entry group =
    { tabs =
        if List.member entry group.tabs then
            group.tabs

        else
            group.tabs ++ [ entry ]
    , current = Just entry
    }


show : Entry -> Group -> Group
show entry group =
    { group | current = Just entry }


{-| The second side never holds what the first could: an empty first side takes
the second's tabs, and the split goes away.
-}
consolidate : Groups -> Groups
consolidate (Groups groups) =
    if List.isEmpty groups.first.tabs then
        Groups
            { first = groups.second
            , second = { tabs = [], current = Nothing }
            , active = First
            }

    else
        Groups groups


uuidOf : Entry -> String
uuidOf entry =
    case entry of
        Files.File uuid ->
            uuid

        Files.Asset uuid ->
            uuid
