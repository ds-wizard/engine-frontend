module Ui.ListField exposing
    ( Row
    , Words
    , row
    , toHtml
    , toHtmlWithWords
    , withRowActions
    , withRowExtras
    , withRowTestId
    )

{-| A field whose value is a list of things: tags on a project, authors on a
document template, the rows of a mapping.

Four functions do this today (`list`, `listWithHeader`, `listWithCustomMsg`,
`viewList`) and they differ in what they take rather than in what they do. Here
the rows are the caller's — each one is whatever fields that row needs — and this
owns what surrounds them: the name of the set, the order, the remove buttons and
the one button that adds another.

Two things it gets right that the existing ones do not:

  - **Every remove button says what it removes.** A column of buttons all called
    "Remove" is a column a screen reader cannot tell apart, and position is not
    something it reads out.
  - **The set is a group.** `role="group"` with the field's name, so entering it
    says what these rows are rather than dropping into an unexplained form.
  - **The hint and the error describe the group.** They are inside the
    `fieldset` and named in its `aria-describedby`, so both are read as the set is
    entered, as they are for a group of choices in `Ui.Field`.
  - **A new item opens on its first field.** `packages/elm-ui/js/list-field.js` watches
    for the item an Add press appends and focuses its first field, so the caret
    is where the typing starts. The application only has to load that script.

-}

import Html exposing (Html, div, fieldset, legend, li, ol, p, span, text)
import Html.Attributes exposing (attribute, class, id)
import Maybe.Extra as Maybe
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.TestId as TestId


type Row msg
    = Row
        { label : String
        , content : List (Html msg)
        , onRemove : Maybe msg
        , actions : List (Button msg)
        , extras : List (Html msg)
        , testId : Maybe String
        }


{-| `label` names this row among the others — a value from it, usually. It is
what the remove button is called after, so "Remove author 2" is what a column of
identical buttons becomes.
-}
row : { label : String, content : List (Html msg) } -> Row msg
row config =
    Row { label = config.label, content = config.content, onRemove = Nothing, actions = [], extras = [], testId = Nothing }


{-| Controls for this item other than removing it — move up, move down. They
sit with the remove button, together at the top end of the item's frame, as
small icon buttons with a tooltip each: every item's controls in the same place,
and never among its fields.
-}
withRowActions : List (Button msg) -> Row msg -> Row msg
withRowActions actions (Row config) =
    Row { config | actions = actions }


{-| A control for this row that is not a plain button — a copy button, which
carries a model and a port of its own and says "Copied!" where it stands. It
sits with the row's other controls, before them, because what it does is read
the row rather than change it.
-}
withRowExtras : List (Html msg) -> Row msg -> Row msg
withRowExtras extras (Row config) =
    Row { config | extras = extras }


toHtml :
    { id : String
    , label : String
    , rows : List (Row msg)
    , addLabel : String
    , onAdd : msg
    , onRemove : Maybe (Int -> msg)
    , hint : Maybe (List (Html msg))
    , error : Maybe String
    }
    -> Html msg
toHtml =
    toHtmlWithWords english


{-| What the remove buttons are called: `remove` is given the row's name, and
`row` names a row that has none of its own by its position, counted from one.
`toHtml` says it in English — "Remove author 2", "row 3".
-}
type alias Words =
    { remove : String -> String
    , row : Int -> String
    }


english : Words
english =
    { remove = \name -> "Remove " ++ name
    , row = \position -> "row " ++ String.fromInt position
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords :
    Words
    ->
        { id : String
        , label : String
        , rows : List (Row msg)
        , addLabel : String
        , onAdd : msg
        , onRemove : Maybe (Int -> msg)
        , hint : Maybe (List (Html msg))
        , error : Maybe String
        }
    -> Html msg
toHtmlWithWords words config =
    let
        hintId =
            config.id ++ "-hint"

        errorId =
            config.id ++ "-error"

        describedBy =
            [ Maybe.map (always hintId) config.hint
            , Maybe.map (always errorId) config.error
            ]
                |> Maybe.values
                |> String.join " "
    in
    fieldset
        (class "ui-list-field"
            :: id config.id
            :: (if String.isEmpty describedBy then
                    []

                else
                    [ attribute "aria-describedby" describedBy ]
               )
        )
        [ legend [ class "ui-list-field__legend" ] [ text config.label ]

        -- An ordered list, because the order is part of the value: a mapping
        -- read back in a different order is a different mapping.
        , ol [ class "ui-list-field__rows" ]
            (List.indexedMap (viewRow words config) config.rows)
        , div [ class "ui-list-field__add" ]
            [ Button.toHtml
                (Button.withIcon (Icon.solid "fa-plus") (Button.secondary config.addLabel config.onAdd)
                    |> Button.small
                )
            ]

        -- A sentence about the set as a whole, and an error about the set
        -- rather than about one row — "at least one author is required".
        , case config.hint of
            Just hint ->
                p [ class "ui-field__hint", id hintId ] hint

            Nothing ->
                text ""
        , case config.error of
            Just error ->
                p [ class "ui-field__error", id errorId ]
                    [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                    , text error
                    ]

            Nothing ->
                text ""
        ]


{-| The row's test hook. Rows are in the order the reader made, so the hook is
the caller's — from what the row holds — rather than its position.
-}
withRowTestId : String -> Row msg -> Row msg
withRowTestId testId (Row config) =
    Row { config | testId = Just testId }


viewRow :
    Words
    -> { a | onRemove : Maybe (Int -> msg) }
    -> Int
    -> Row msg
    -> Html msg
viewRow words config index (Row entry) =
    let
        -- The same round danger button as a Delete on a table row: one
        -- destructive action among many. Named after what it removes —
        -- "Remove" on its own is what a screen reader hears five times in a
        -- row.
        remove =
            case config.onRemove of
                Just onRemove ->
                    [ Button.dangerIconOnly (Icon.solid "fa-trash")
                        (words.remove (rowName words entry.label index))
                        (onRemove index)
                    ]

                Nothing ->
                    []

        controls =
            entry.actions ++ remove
    in
    li (class "ui-list-field__row" :: TestId.attributes entry.testId)
        [ div [ class "ui-list-field__content" ] entry.content
        , if List.isEmpty controls && List.isEmpty entry.extras then
            text ""

          else
            -- Small, with the label as a tooltip, as a table row's actions are.
            div [ class "ui-list-field__actions" ]
                (entry.extras
                    ++ List.map (Button.toHtml << Button.withTooltip << Button.small) controls
                )
        ]


{-| A row with nothing in it yet has no name of its own, so it is named by where
it is — which is worse than a value and better than nothing.
-}
rowName : Words -> String -> Int -> String
rowName words label index =
    if String.isEmpty (String.trim label) then
        words.row (index + 1)

    else
        label
