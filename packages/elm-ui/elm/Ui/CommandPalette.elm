module Ui.CommandPalette exposing
    ( Item, item, withIcon, withLeading, matches
    , Model, init, query, Msg, Outcome(..), update, dismissed
    , Words, english
    , view
    )

{-| Somewhere to type where you want to go.

One box over everything, opened from the keyboard from wherever you are, that
takes a few letters and offers what they name. The application decides what is
in it; this owns the typing, the cursor, the keys and the markup.


# What is in it

The caller passes the items to show, already narrowed to the query — `matches`
is what this would use, and is exposed so the caller can use the same rule. That
seam is deliberate: today the items are the pages whose addresses the
application already knows, and a list that also comes from the server is the
same view with more items in it and `loading` set while they are on their way.
Nothing here needs to change for that.

@docs Item, item, withIcon, withLeading, matches


# State

@docs Model, init, query, Msg, Outcome, update, dismissed


# Words

@docs Words, english


# View

@docs view

-}

import Browser.Dom as Dom
import Html exposing (Html, div, h2, input, li, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, preventDefaultOn)
import Json.Decode as Decode
import Task
import Ui.Icon as Icon exposing (Icon)



-- ITEMS


{-| Somewhere to go: what it is called, the heading it sits under, and the
address. The id has to be unique in the palette — the cursor is a reference to
it.
-}
type Item
    = Item
        { id : String
        , label : String
        , section : String
        , url : String
        , leading : Maybe Leading
        }


type Leading
    = Glyph Icon
    | Drawn (Html Never)


item : { id : String, label : String, section : String, url : String } -> Item
item config =
    Item
        { id = config.id
        , label = config.label
        , section = config.section
        , url = config.url
        , leading = Nothing
        }


{-| Whether an item is one of the ones a query names: every word typed is
somewhere in its label or in the name of its section, in any order and in any
case. Typing two words that are both in the label narrows; typing the section's
name and a word of the label finds it too.

The palette does not apply this itself — the caller does, because the caller is
the one that will one day be merging these items with a server's.

-}
matches : String -> Item -> Bool
matches queryText (Item config) =
    let
        haystack =
            String.toLower (config.label ++ " " ++ config.section)
    in
    queryText
        |> String.toLower
        |> String.words
        |> List.all (\word -> String.contains word haystack)


{-| The glyph the page carries elsewhere — the one beside it in the navigation.
Decorative: the label is what names the item.
-}
withIcon : Icon -> Item -> Item
withIcon icon (Item config) =
    Item { config | leading = Just (Glyph icon) }


{-| A mark drawn by the application in place of a glyph — the icon of a place it
has, such as a workspace's, as it looks everywhere else. Decorative as well.
-}
withLeading : Html Never -> Item -> Item
withLeading mark (Item config) =
    Item { config | leading = Just (Drawn mark) }



-- STATE


type Model
    = Model
        { query : String
        , highlighted : Int
        }


init : Model
init =
    Model { query = "", highlighted = 0 }


{-| What has been typed, for the caller to narrow its own items by.
-}
query : Model -> String
query (Model model) =
    model.query


type Msg
    = QueryChanged String
    | Moved Int
    | Hovered Int
    | Chose String
    | Dismissed
    | Noop


{-| What the palette asks the application to do. Navigating closes it: the
palette is a way to leave, and one still open over the page it just opened would
have to be dismissed by hand.
-}
type Outcome
    = Navigate String
    | Close


{-| The items are the ones on screen, because Enter takes the one the cursor is
on and the cursor is a position in that list.

The command keeps the item the keys moved the cursor to in view: the list
scrolls, and an arrow pressed at its edge would otherwise move the cursor to
somewhere nobody can see.

-}
update : List Item -> Msg -> Model -> ( Model, Cmd Msg, Maybe Outcome )
update items msg (Model model) =
    case msg of
        QueryChanged value ->
            -- Back to the top: the first item under the new query is rarely the
            -- one that was under the cursor before it.
            ( Model { model | query = value, highlighted = 0 }, Cmd.none, Nothing )

        Moved position ->
            let
                highlighted =
                    wrap position (List.length items)
            in
            ( Model { model | highlighted = highlighted }, scrollIntoView items highlighted, Nothing )

        Hovered position ->
            -- Not scrolled to: the item is under the pointer, so it is in view,
            -- and a list that moved under a resting mouse would pick another.
            ( Model { model | highlighted = wrap position (List.length items) }, Cmd.none, Nothing )

        Chose url ->
            ( Model model, Cmd.none, Just (Navigate url) )

        Dismissed ->
            ( Model model, Cmd.none, Just Close )

        Noop ->
            ( Model model, Cmd.none, Nothing )


{-| The message that closes it, for the application to send when the dialog
says it has been dismissed — the constructors are not exposed, because every
other one of them is the palette's own business.
-}
dismissed : Msg
dismissed =
    Dismissed


{-| Scrolls the list only as far as it takes to show the item, and only when it
is not shown already — aligning it to an edge on every press would yank the list
on each arrow.

The first item of a section brings its section's name with it, so going up to
it shows what it is under rather than stopping a line short; the first of all
scrolls the list to its very top.

-}
scrollIntoView : List Item -> Int -> Cmd Msg
scrollIntoView items highlighted =
    if highlighted == 0 then
        -- The very top, the list's own padding with it.
        Task.attempt (always Noop) (Dom.setViewportOf listboxId 0 0)

    else
        scrollIntoViewBelowTop items highlighted


scrollIntoViewBelowTop : List Item -> Int -> Cmd Msg
scrollIntoViewBelowTop items highlighted =
    case List.drop (highlighted - 1) items |> List.take 2 of
        [ Item previous, Item current ] ->
            if previous.section == current.section then
                scrollTo (optionId current.id) (optionId current.id)

            else
                scrollTo (sectionId current.id) (optionId current.id)

        _ ->
            Cmd.none


{-| From the top of one element to the bottom of the other, measured in the
list's own scrolled content.
-}
scrollTo : String -> String -> Cmd Msg
scrollTo topId bottomId =
    Task.map3
        (\top bottom list ->
            Dom.getViewportOf listboxId
                |> Task.andThen
                    (\viewport ->
                        let
                            offset element =
                                viewport.viewport.y + element.element.y - list.element.y

                            start =
                                offset top
                        in
                        if start < viewport.viewport.y then
                            Dom.setViewportOf listboxId 0 start

                        else
                            let
                                end =
                                    offset bottom + bottom.element.height
                            in
                            if end > viewport.viewport.y + viewport.viewport.height then
                                Dom.setViewportOf listboxId 0 (end - viewport.viewport.height)

                            else
                                Task.succeed ()
                    )
        )
        (Dom.getElement topId)
        (Dom.getElement bottomId)
        (Dom.getElement listboxId)
        |> Task.andThen identity
        |> Task.attempt (always Noop)


wrap : Int -> Int -> Int
wrap position count =
    if count == 0 then
        0

    else
        modBy count position



-- WORDS


type alias Words =
    { title : String
    , placeholder : String
    , empty : String
    , loading : String
    }


english : Words
english =
    { title = "Go to"
    , placeholder = "Where to?"
    , empty = "Nothing matches what you typed."
    , loading = "Searching…"
    }



-- VIEW


{-| `open` is the application's, not the palette's: what opens it is a shortcut
the application owns, and the dialog has to be told to show itself.

`items` are shown in the order given, grouped by their section in that order
too. `loading` is for the day some of them come from a server: it says so under
what is already there rather than replacing it, because what is already there is
still worth choosing.

-}
view :
    { open : Bool
    , items : List Item
    , loading : Bool
    , words : Words
    , wrapMsg : Msg -> msg
    , onClose : msg
    }
    -> Model
    -> Html msg
view config (Model model) =
    let
        openAttributes =
            if config.open then
                [ attribute "open" "" ]

            else
                []

        count =
            List.length config.items

        highlighted =
            wrap model.highlighted count
    in
    Html.node "ui-dialog"
        (attribute "data-variant" "palette"
            :: Html.Events.on "dialogClose" (Decode.succeed config.onClose)
            :: openAttributes
        )
        [ Html.node "dialog"
            [ class "ui-dialog", attribute "aria-labelledby" titleId ]
            [ div [ class "ui-command-palette" ]
                [ h2 [ class "ui-visually-hidden", id titleId ] [ text config.words.title ]
                , div [ class "ui-command-palette__search" ]
                    [ span [ class "ui-command-palette__search-icon", attribute "aria-hidden" "true" ]
                        [ Icon.toHtml (Icon.solid "fa-magnifying-glass") ]
                    , input
                        [ type_ "text"
                        , id inputId
                        , class "ui-input ui-command-palette__input"
                        , value model.query
                        , placeholder config.words.placeholder
                        , autocomplete False
                        , attribute "role" "combobox"
                        , attribute "aria-expanded" "true"
                        , attribute "aria-controls" listboxId
                        , attribute "aria-autocomplete" "list"
                        , attribute "aria-label" config.words.title
                        , activeDescendant config.items highlighted
                        , onInput (config.wrapMsg << QueryChanged)
                        , onKeyboard config highlighted
                        ]
                        []
                    ]
                , if count == 0 && not config.loading then
                    div [ class "ui-command-palette__empty" ] [ text config.words.empty ]

                  else
                    ul
                        [ class "ui-command-palette__list"
                        , id listboxId
                        , attribute "role" "listbox"
                        , attribute "aria-label" config.words.title
                        , attribute "aria-busy"
                            (if config.loading then
                                "true"

                             else
                                "false"
                            )
                        ]
                        (viewSections config highlighted)
                , if config.loading then
                    div [ class "ui-command-palette__loading", attribute "role" "status" ] [ text config.words.loading ]

                  else
                    text ""
                ]
            ]
        ]


{-| The sections in the order their first item appears, each a group inside the
listbox — a heading that is read as the name of the set rather than as an option
nobody can choose.
-}
viewSections :
    { a | items : List Item, wrapMsg : Msg -> msg }
    -> Int
    -> List (Html msg)
viewSections config highlighted =
    let
        indexed =
            List.indexedMap Tuple.pair config.items

        sections =
            List.foldr
                (\( index, Item current ) grouped ->
                    case grouped of
                        ( name, entries ) :: rest ->
                            if name == current.section then
                                ( name, ( index, Item current ) :: entries ) :: rest

                            else
                                ( current.section, [ ( index, Item current ) ] ) :: grouped

                        [] ->
                            [ ( current.section, [ ( index, Item current ) ] ) ]
                )
                []
                indexed
    in
    List.map
        (\( name, entries ) ->
            li
                (class "ui-command-palette__section"
                    :: attribute "role" "presentation"
                    :: (case entries of
                            ( _, Item first ) :: _ ->
                                [ id (sectionId first.id) ]

                            [] ->
                                []
                       )
                )
                [ span [ class "ui-command-palette__section-name" ] [ text name ]
                , ul
                    [ class "ui-command-palette__section-items"
                    , attribute "role" "group"
                    , attribute "aria-label" name
                    ]
                    (List.map (viewItem config highlighted) entries)
                ]
        )
        sections


viewItem : { a | wrapMsg : Msg -> msg } -> Int -> ( Int, Item ) -> Html msg
viewItem config highlighted ( index, Item current ) =
    li
        [ id (optionId current.id)
        , attribute "role" "option"
        , attribute "aria-selected"
            (if index == highlighted then
                "true"

             else
                "false"
            )
        , classList
            [ ( "ui-command-palette__item", True )
            , ( "is-highlighted", index == highlighted )
            ]

        -- The press must not take focus off the field the cursor is steered
        -- from; choosing happens on release (WCAG 2.5.2), so sliding off before
        -- letting go cancels it.
        , preventDefaultOn "mousedown" (Decode.succeed ( config.wrapMsg Noop, True ))
        , onClick (config.wrapMsg (Chose current.url))
        , onMouseEnter (config.wrapMsg (Hovered index))
        ]
        (case current.leading of
            Just leading ->
                [ span [ class "ui-command-palette__item-icon", attribute "aria-hidden" "true" ]
                    [ case leading of
                        Glyph icon ->
                            Icon.toHtml icon

                        Drawn mark ->
                            Html.map never mark
                    ]
                , span [ class "ui-command-palette__item-label" ] [ text current.label ]
                ]

            Nothing ->
                [ span [ class "ui-command-palette__item-label" ] [ text current.label ] ]
        )


{-| The cursor is a reference rather than focus: focus stays in the field being
typed into, which is the only way the arrows and the letters can both work.
-}
activeDescendant : List Item -> Int -> Html.Attribute msg
activeDescendant items highlighted =
    case List.drop highlighted items of
        (Item current) :: _ ->
            attribute "aria-activedescendant" (optionId current.id)

        [] ->
            attribute "aria-activedescendant" ""


onKeyboard : { a | items : List Item, wrapMsg : Msg -> msg, onClose : msg } -> Int -> Html.Attribute msg
onKeyboard config highlighted =
    preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowDown" ->
                            Decode.succeed ( config.wrapMsg (Moved (highlighted + 1)), True )

                        "ArrowUp" ->
                            Decode.succeed ( config.wrapMsg (Moved (highlighted - 1 + List.length config.items)), True )

                        "Home" ->
                            Decode.succeed ( config.wrapMsg (Moved 0), True )

                        "End" ->
                            Decode.succeed ( config.wrapMsg (Moved (List.length config.items - 1)), True )

                        "Enter" ->
                            case List.drop highlighted config.items of
                                (Item current) :: _ ->
                                    Decode.succeed ( config.wrapMsg (Chose current.url), True )

                                [] ->
                                    Decode.fail "nothing under the cursor"

                        _ ->
                            -- Escape belongs to the dialog, which closes itself
                            -- and says so; every other key is typing.
                            Decode.fail "not a key the palette takes"
                )
        )


inputId : String
inputId =
    "ui-command-palette-input"


listboxId : String
listboxId =
    "ui-command-palette-list"


titleId : String
titleId =
    "ui-command-palette-title"


{-| A section is known by its first item, which is unique in the palette.
-}
sectionId : String -> String
sectionId firstItemId =
    "ui-command-palette-section-" ++ firstItemId


optionId : String -> String
optionId itemId =
    "ui-command-palette-item-" ++ itemId
