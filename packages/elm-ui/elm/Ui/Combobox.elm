module Ui.Combobox exposing
    ( Config
    , Model
    , Msg
    , clear
    , init
    , isOpen
    , query
    , resultsWanted
    , select
    , selected
    , setSelected
    , toField
    , toFieldWithHiddenLabel
    , toHtml
    , update
    )

{-| An input that suggests items from an API — the "typehint input" of the
existing codebase, rebuilt to reach assistive technology.

The interaction is the existing one and it is a good one: once something is
chosen the control shows the **value** rather than a text field, and clicking or
focusing it reopens the search. That is a select with a filter, not a plain text
box, and the markup follows suit — a trigger button that owns a popup, with the
search field inside the popup.

What is added is everything that was silent before. The trigger carries
`aria-haspopup` and `aria-expanded`; the search field is the `combobox` and
moves a virtual cursor with `aria-activedescendant` while focus stays put; the
options are a real `listbox`. Previously the options were anchors without
`href`, so a screen reader never learned the suggestions existed.


## Suggestions from the server

The options are the caller's on every render, so a list that comes back from an
API is not a different mode of the component — it is the same one with
`loading` set while the request is out. What changes is what has to be said:

  - While loading, the listbox carries `aria-busy` and the search field's own
    glyph turns into a spinner, named with `loadingLabel`. Nothing is written
    under the options: a row that says "Loading" is not something to choose,
    and beside a spinner it is the same news twice.
  - The number of results is announced in a polite live region, through
    `resultsLabel`. The options arrive while focus is in the search field and
    nothing there moves, so without it the only way to learn the request has
    come back is to press an arrow key and see.
  - `emptyLabel` is shown only once nothing is loading. "No users match" while
    the request is still out is a claim the component cannot yet make. A failed
    request says so here too — the caller passes the error as `emptyLabel` —
    which is why the popup still draws this row when the search field's spinner
    has stopped.

`resultsWanted` is how the caller learns it should ask. It compares the model
before and after `update` rather than inspecting the message, so `update` keeps
its signature and the caller does not have to know which of the several ways in
— a click, a focus, typing — opened the popup.

-}

import Browser.Dom as Dom
import Html exposing (Html, button, div, input, li, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, class, classList, id, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter, preventDefaultOn)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Ui.Icon as Icon
import Ui.TestId as TestId


type Model a
    = Model
        { query : String
        , open : Bool
        , highlighted : Int
        , chosen : Maybe a

        -- Escape closes and returns focus to the trigger. Without this the
        -- trigger's own focus handler would reopen it immediately.
        , reopenBlocked : Bool
        }


init : Model a
init =
    Model
        { query = ""
        , open = False
        , highlighted = 0
        , chosen = Nothing
        , reopenBlocked = False
        }


selected : Model a -> Maybe a
selected (Model model) =
    model.chosen


{-| What the user has typed. The caller owns the results, so it needs this to
ask the API.
-}
query : Model a -> String
query (Model model) =
    model.query


{-| Whether the popup is showing. Useful to a caller that needs to hold off on
a request, or to close the popup when something else takes over the screen.
-}
isOpen : Model a -> Bool
isOpen (Model model) =
    model.open


{-| Choose an item without the user picking it — preselecting from a route
parameter, say. Goes through `update` so the closing behavior is the same one
a click gets, rather than a second path that could drift from it.
-}
select : a -> Msg a
select =
    Chose


{-| The query to fetch results for, when there is a new one.

`Just query` when the popup has just opened, or when what was typed changed
while it was open; `Nothing` otherwise. Opening counts, because an async list
has nothing in it until it is asked — the first suggestions should be there
without anyone having to type.

    ( next, cmd ) =
        Combobox.update fieldId count subMsg model.users

    fetch =
        case Combobox.resultsWanted { before = model.users, after = next } of
            Just typed ->
                searchUsers typed

            Nothing ->
                Cmd.none

Responses can arrive out of order. Keep a result only while its query is still
`Combobox.query` of the current model; debouncing is the caller's as well.

-}
resultsWanted : { before : Model a, after : Model a } -> Maybe String
resultsWanted { before, after } =
    let
        (Model old) =
            before

        (Model new) =
            after
    in
    if new.open && (not old.open || old.query /= new.query) then
        Just new.query

    else
        Nothing


clear : Model a -> Model a
clear (Model model) =
    Model { model | chosen = Nothing, query = "", open = False, highlighted = 0 }


{-| Put a value in the control without anyone choosing it and without moving
focus — the value a form was loaded with, say.

`select` goes through `update` and hands focus back to the trigger, which is
right after a choice and wrong on a page that has just loaded: focus would jump
to a field nobody asked for.

-}
setSelected : Maybe a -> Model a -> Model a
setSelected value (Model model) =
    Model { model | chosen = value }


type Msg a
    = Opened
    | FocusedTrigger
    | BlurredTrigger
    | Closed
    | Dismissed
    | QueryChanged String
    | HighlightMoved Int
    | Highlighted Int
    | Chose a
    | Cleared
    | Noop


update : String -> Int -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update fieldId count msg (Model model) =
    case msg of
        Opened ->
            ( Model { model | open = True, highlighted = 0 }, focusSearch fieldId )

        FocusedTrigger ->
            if model.reopenBlocked then
                ( Model model, Cmd.none )

            else
                ( Model { model | open = True, highlighted = 0 }, focusSearch fieldId )

        BlurredTrigger ->
            ( Model { model | reopenBlocked = False }, Cmd.none )

        Closed ->
            ( Model { model | open = False, highlighted = 0 }, Cmd.none )

        Dismissed ->
            closeReturningFocus fieldId model

        QueryChanged typed ->
            ( Model { model | query = typed, highlighted = 0 }, Cmd.none )

        HighlightMoved index ->
            ( Model { model | highlighted = wrap count index }
            , scrollOptionIntoView fieldId (wrap count index)
            )

        Highlighted index ->
            -- Pointer and keyboard share one highlight, so moving the mouse
            -- does not leave a second, stale cursor behind.
            ( Model { model | highlighted = index }, Cmd.none )

        Chose item ->
            closeReturningFocus fieldId { model | chosen = Just item, query = "" }

        Cleared ->
            closeReturningFocus fieldId { model | chosen = Nothing, query = "" }

        Noop ->
            ( Model model, Cmd.none )


{-| Closing and handing focus back to the trigger are one operation, not two.

The trigger opens on focus, so anything that focuses it would reopen the popup
the user just left — after choosing an item, after clearing, after Escape. The
block is lifted when the trigger is blurred.

Keeping this in one place is the point: every path that returns focus has to set
the flag, and three separate call sites would eventually miss one.

-}
closeReturningFocus :
    String
    ->
        { query : String
        , open : Bool
        , highlighted : Int
        , chosen : Maybe a
        , reopenBlocked : Bool
        }
    -> ( Model a, Cmd (Msg a) )
closeReturningFocus fieldId model =
    ( Model { model | open = False, highlighted = 0, reopenBlocked = True }
    , focusTrigger fieldId
    )


wrap : Int -> Int -> Int
wrap count index =
    if count == 0 then
        0

    else
        modBy count index


focusSearch : String -> Cmd (Msg a)
focusSearch fieldId =
    Task.attempt (always Noop) (Dom.focus (searchElementId fieldId))


focusTrigger : String -> Cmd (Msg a)
focusTrigger fieldId =
    Task.attempt (always Noop) (Dom.focus (triggerElementId fieldId))


{-| Keeps the highlighted option in view without moving focus, which stays in
the search field for the whole interaction.

Scrolls only when the option is actually outside the visible area, and only far
enough to bring it in. Aligning to the top on every press would yank the list on
each arrow key, which is the usual way this reads wrong.

-}
scrollOptionIntoView : String -> Int -> Cmd (Msg a)
scrollOptionIntoView fieldId index =
    Dom.getElement (optionElementId fieldId index)
        |> Task.andThen
            (\option ->
                Dom.getElement (listboxElementId fieldId)
                    |> Task.andThen
                        (\list ->
                            Dom.getViewportOf (listboxElementId fieldId)
                                |> Task.andThen
                                    (\viewport ->
                                        let
                                            -- Where the option sits relative to
                                            -- the top of the scrollable content.
                                            optionTop =
                                                viewport.viewport.y + option.element.y - list.element.y
                                        in
                                        if optionTop < viewport.viewport.y then
                                            Dom.setViewportOf (listboxElementId fieldId) 0 optionTop

                                        else if optionTop + option.element.height > viewport.viewport.y + list.element.height then
                                            Dom.setViewportOf (listboxElementId fieldId)
                                                0
                                                (optionTop + option.element.height - list.element.height)

                                        else
                                            Task.succeed ()
                                    )
                        )
            )
        |> Task.attempt (always Noop)


{-| Standalone, the trigger is named by `aria-label`. In a field it is named by
the visible label and its own content, and described by the hint and error.
-}
triggerNaming : Maybe FieldInfo -> Config a -> List (Html.Attribute (Msg a))
triggerNaming field config =
    case field of
        Nothing ->
            [ attribute "aria-label" config.label ]

        Just info ->
            let
                describedBy =
                    Maybe.values
                        [ Maybe.map (always (hintElementId config.id)) info.hint
                        , Maybe.map (always (errorElementId config.id)) info.error
                        ]
            in
            attribute "aria-labelledby" (labelElementId config.id ++ " " ++ triggerElementId config.id)
                :: (if List.isEmpty describedBy then
                        []

                    else
                        [ attribute "aria-describedby" (String.join " " describedBy) ]
                   )
                ++ (if Maybe.isJust info.error then
                        [ attribute "aria-invalid" "true" ]

                    else
                        []
                   )


labelElementId : String -> String
labelElementId fieldId =
    fieldId ++ "-label"


hintElementId : String -> String
hintElementId fieldId =
    fieldId ++ "-hint"


errorElementId : String -> String
errorElementId fieldId =
    fieldId ++ "-error"


triggerElementId : String -> String
triggerElementId fieldId =
    fieldId ++ "-trigger"


searchElementId : String -> String
searchElementId fieldId =
    fieldId ++ "-search"


listboxElementId : String -> String
listboxElementId fieldId =
    fieldId ++ "-listbox"


optionElementId : String -> Int -> String
optionElementId fieldId index =
    fieldId ++ "-option-" ++ String.fromInt index


type alias Config a =
    { id : String
    , label : String
    , placeholder : String
    , searchPlaceholder : String
    , options : List a
    , viewOption : a -> Html (Msg a)
    , optionTestId : a -> String
    , viewSelected : a -> Html (Msg a)
    , loading : Bool
    , loadingLabel : String
    , emptyLabel : String
    , resultsLabel : Int -> String
    , clearLabel : Maybe String
    }


toHtml : Config a -> Model a -> Html (Msg a)
toHtml config model =
    view Nothing config model


{-| The combobox as a form field: a visible label, a hint, an error, and the
required mark, laid out and wired the way `Ui.Field` wires every other control.

It is not a `Ui.Field` constructor because the field would then have to own the
combobox's model and messages, and `Ui.Field` is stateless. The markup is
`Ui.Field`'s own, so the two cannot be told apart on the page.

What changes on the trigger, which is where focus rests:

  - It is named by the label **and** by its own content —
    `aria-labelledby="<label> <trigger>"` — so a screen reader says
    "Knowledge model, Horizon Europe DMP" rather than the label alone, which is
    what `aria-label` on a button whose text is the value does.
  - The hint and the error are in its `aria-describedby`, and an error sets
    `aria-invalid`, so they are heard where the value is rather than only seen
    under it.
  - Required is the visible mark plus `aria-required` on the search field. A
    `button` cannot carry `aria-required`; the `combobox` it opens can.

-}
toField :
    { hint : Maybe String, error : Maybe String, required : Bool }
    -> Config a
    -> Model a
    -> Html (Msg a)
toField field config model =
    toFieldWithHiddenLabel False field config model


{-| `toField`, with the label there for a screen reader and not drawn.

For a combobox in a toolbar, where the row itself says what the controls are
for and a label over each one would make the row three lines tall. The label is
still the control's name — it is hidden, not removed (WCAG 3.3.2, 1.3.1).

-}
toFieldWithHiddenLabel :
    Bool
    -> { hint : Maybe String, error : Maybe String, required : Bool }
    -> Config a
    -> Model a
    -> Html (Msg a)
toFieldWithHiddenLabel hiddenLabel field config model =
    div [ class "ui-field" ]
        [ Html.label
            [ classList
                [ ( "ui-field__label", True )
                , ( "ui-visually-hidden", hiddenLabel )
                ]
            , id (labelElementId config.id)
            , Html.Attributes.for (triggerElementId config.id)
            ]
            [ text config.label
            , if field.required then
                span [ class "ui-field__required", attribute "aria-hidden" "true" ] [ text "*" ]

              else
                text ""
            ]
        , view (Just field) config model
        , case field.hint of
            Just hint ->
                Html.p [ class "ui-field__hint", id (hintElementId config.id) ] [ text hint ]

            Nothing ->
                text ""
        , case field.error of
            Just error ->
                Html.p [ class "ui-field__error", id (errorElementId config.id) ]
                    [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                    , text error
                    ]

            Nothing ->
                text ""
        ]


type alias FieldInfo =
    { hint : Maybe String, error : Maybe String, required : Bool }


view : Maybe FieldInfo -> Config a -> Model a -> Html (Msg a)
view field config (Model model) =
    div [ class "ui-combobox", id config.id ]
        [ viewTrigger field config (Model model)
        , if model.open then
            viewPopup field config (Model model)

          else
            text ""
        ]


{-| Closed state: the chosen value, not a text field.
-}
viewTrigger : Maybe FieldInfo -> Config a -> Model a -> Html (Msg a)
viewTrigger field config (Model model) =
    let
        clearable =
            Maybe.isJust model.chosen && Maybe.isJust config.clearLabel
    in
    div
        [ classList
            [ ( "ui-combobox__trigger-row", True )
            , ( "is-clearable", clearable )
            ]
        ]
        [ button
            ([ type_ "button"
             , id (triggerElementId config.id)
             , classList
                [ ( "ui-combobox__trigger", True )
                , ( "is-open", model.open )
                , ( "is-empty", Maybe.isNothing model.chosen )
                , ( "is-invalid", Maybe.isJust (Maybe.andThen .error field) )
                ]
             , attribute "aria-haspopup" "listbox"
             , attribute "aria-expanded"
                (if model.open then
                    "true"

                 else
                    "false"
                )
             , onClick Opened
             , onFocus FocusedTrigger
             , onBlur BlurredTrigger
             ]
                ++ triggerNaming field config
            )
            [ case model.chosen of
                Just item ->
                    config.viewSelected item

                Nothing ->
                    span [ class "ui-combobox__placeholder" ] [ text config.placeholder ]
            ]
        , case ( model.chosen, config.clearLabel ) of
            ( Just _, Just clearLabel ) ->
                button
                    [ type_ "button"
                    , class "ui-combobox__clear"
                    , attribute "aria-label" clearLabel
                    , onClick Cleared
                    ]
                    [ Icon.toHtml (Icon.solid "fa-xmark") ]

            _ ->
                text ""
        ]


{-| The search field, the listbox, and what is said about them.

The listbox is always rendered, even empty, because the search field's
`aria-controls` points at it: a reference to an element that comes and goes is a
reference to nothing half the time.

-}
viewPopup : Maybe FieldInfo -> Config a -> Model a -> Html (Msg a)
viewPopup field config (Model model) =
    let
        count =
            List.length config.options
    in
    div [ class "ui-combobox__popup" ]
        [ div [ class "ui-combobox__search" ]
            [ span [ class "ui-combobox__search-icon" ]
                [ if config.loading then
                    -- Named, because it is the only thing saying a request is
                    -- out: the row that used to say so in words is gone, the
                    -- glyph turning into a spinner having said it already.
                    Icon.toHtml (Icon.labeled config.loadingLabel Icon.spinner)

                  else
                    Icon.toHtml (Icon.solid "fa-magnifying-glass")
                ]
            , input
                [ type_ "text"
                , id (searchElementId config.id)
                , class "ui-input"
                , value model.query
                , Html.Attributes.placeholder config.searchPlaceholder
                , autocomplete False
                , attribute "role" "combobox"
                , attribute "aria-expanded" "true"
                , attribute "aria-controls" (listboxElementId config.id)
                , attribute "aria-autocomplete" "list"
                , attribute "aria-label" config.label
                , attribute "aria-required" (boolToString (Maybe.map .required field == Just True))
                , attribute "aria-activedescendant"
                    -- Only an option that is there. Results that come back
                    -- shorter than the ones before must not leave the cursor
                    -- pointing past the end of the list.
                    (if model.highlighted < count then
                        optionElementId config.id model.highlighted

                     else
                        ""
                    )
                , onInput QueryChanged
                , onBlur Closed
                , onKeyboard model.highlighted count config.options
                ]
                []
            ]
        , ul
            [ class "ui-combobox__listbox"
            , id (listboxElementId config.id)
            , attribute "role" "listbox"
            , attribute "aria-label" config.label
            , attribute "aria-busy" (boolToString config.loading)
            ]
            (List.indexedMap (viewOption config model.highlighted) config.options)
        , if config.loading then
            -- Nothing. The glyph in the search field turns into a spinner while
            -- a request is out, and a row saying the same thing in words under
            -- an empty box is the same news twice. `aria-busy` on the listbox
            -- and the name on the spinner carry it for anyone not watching it
            -- turn.
            text ""

          else if count == 0 then
            -- Seen here, heard from the live region below. Hidden so that
            -- reading through the popup does not say it twice.
            div [ class "ui-combobox__empty", attribute "aria-hidden" "true" ]
                [ text config.emptyLabel ]

          else
            text ""
        , div
            [ class "ui-visually-hidden"
            , attribute "role" "status"
            , attribute "aria-live" "polite"
            , attribute "aria-atomic" "true"
            ]
            [ text (announcement config count) ]
        ]


{-| What the live region says: nothing while a request is out, then the count.

Empty while loading on purpose. A live region is announced when its text
changes, so a search that comes back with the same five results as the last one
still says "5 results" — it went from nothing to that, not from that to itself.

-}
announcement : Config a -> Int -> String
announcement config count =
    if config.loading then
        ""

    else if count == 0 then
        config.emptyLabel

    else
        config.resultsLabel count


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


viewOption : Config a -> Int -> Int -> a -> Html (Msg a)
viewOption config highlighted index option =
    li
        [ id (optionElementId config.id index)
        , TestId.attribute (config.optionTestId option)
        , attribute "role" "option"
        , attribute "aria-selected"
            (if index == highlighted then
                "true"

             else
                "false"
            )
        , classList
            [ ( "ui-combobox__option", True )
            , ( "is-highlighted", index == highlighted )
            ]

        -- The press only prevents the default focus shift, so the search
        -- field never blurs and the popup is still open when the click
        -- arrives. Choosing happens on release: WCAG 2.5.2 requires that no
        -- part of the action runs on the down-event, so that sliding off the
        -- option before releasing cancels it.
        , preventDefaultOn "mousedown" (Decode.succeed ( Noop, True ))
        , onClick (Chose option)
        , onMouseEnter (Highlighted index)
        ]
        [ config.viewOption option ]


onKeyboard : Int -> Int -> List a -> Html.Attribute (Msg a)
onKeyboard highlighted count options =
    preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowDown" ->
                            Decode.succeed ( HighlightMoved (highlighted + 1), True )

                        "ArrowUp" ->
                            Decode.succeed ( HighlightMoved (highlighted - 1 + count), True )

                        "Home" ->
                            Decode.succeed ( HighlightMoved 0, True )

                        "End" ->
                            Decode.succeed ( HighlightMoved (count - 1), True )

                        "Escape" ->
                            Decode.succeed ( Dismissed, True )

                        "Enter" ->
                            case List.getAt highlighted options of
                                Just option ->
                                    Decode.succeed ( Chose option, True )

                                Nothing ->
                                    Decode.fail "nothing highlighted"

                        _ ->
                            Decode.fail "not a combobox key"
                )
        )
