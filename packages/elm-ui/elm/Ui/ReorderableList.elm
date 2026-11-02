module Ui.ReorderableList exposing
    ( Config
    , Dragging
    , Item
    , Words
    , item
    , toHtml
    , toHtmlWithWords
    )

{-| An ordered list whose order is part of what it means, and can be changed.

The knowledge model editor's chapters, questions, answers, choices, references,
experts, metrics, phases, tags, integrations and resource pages are all this:
a list where being third rather than fourth is the content, not a view setting.

**Not `Ui.ListField`.** That is a field whose value is a list — rows of inputs
inside a `fieldset`, with an Add button and a legend naming the set. These are
not fields: each row is a place to go, and what surrounds them is a heading and
a tree, not a form.

**Moving is a pair of buttons, and a drag as well.** Everything that reorders in
the wizard does it by dragging and by nothing else, which a keyboard cannot do
at all (2.1.1) and a pointer with poor aim can barely do (2.5.7). Here every row
carries Move up and Move down; they are real buttons, each named after the row
it moves ("Move Data description up"), because a column of buttons all called
"Move up" is a column a screen reader cannot tell apart.

A caller that passes `drag` also gets a handle at the start of each row, which
drags the row onto another — quicker than pressing Move down four times, for
whoever can use it. It is an addition and never the only way: the handle is
hidden from assistive technology, since the buttons beside it already say what
it does, and everything works with it switched off.

**What a drag shows is where the row will land**, as an empty frame opened
between the rows it will land between — not a highlight on the row under the
pointer, which says "this one" when what is being answered is "where". The row
being dragged fades in the place it is leaving.

The drag's state is the caller's, because it is a thing being done rather than
a thing being shown. It names the list it belongs to, so a form holding several
of these — a question's answers, its references, its experts — has one of them
respond to a drag and not all three.

**The add button belongs to the list.** A list one can reorder is nearly always
a list one can add to, and an Add button left to the caller ends up stretched
across the form by whatever lays the form out. It sits under the rows, the width
of its own label.

**The move is announced.** The caller passes what to say after a move —
"Data description is now 2 of 5" — and it is read politely, because a change of
order is invisible to somebody who cannot see the list move. The string has to
differ from the last one for a live region to say it at all, which is why it
carries the position.

**The ends are disabled with a reason.** The first row's Move up and the last
row's Move down are disabled rather than missing, so the control does not move
about under the pointer as rows are shuffled, and each says why it cannot be
pressed.

-}

import Html exposing (Html, div, li, ol, span, text)
import Html.Attributes exposing (attribute, class, classList, draggable)
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as D
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.TestId as TestId


type Item msg
    = Item
        { key : String
        , label : String
        , content : List (Html msg)
        }


{-| `label` names the row among its siblings — its title, usually. It is what
the move buttons are called after.
-}
item : { key : String, label : String, content : List (Html msg) } -> Item msg
item config =
    Item { key = config.key, label = config.label, content = config.content }


{-| A drag in progress: which list it is in (the `id` of the one that started
it), the row it started on and the row the pointer is over. The two are indexes
into that list as it stands.
-}
type alias Dragging =
    { list : String
    , from : Int
    , over : Int
    }


{-| What the controls say. `moveUp` and `moveDown` are given the row's label;
`atStart` and `atEnd` say why a disabled one cannot be pressed.
-}
type alias Words =
    { moveUp : String -> String
    , moveDown : String -> String
    , atStart : String
    , atEnd : String
    }


english : Words
english =
    { moveUp = \label -> "Move " ++ label ++ " up"
    , moveDown = \label -> "Move " ++ label ++ " down"
    , atStart = "It is already first"
    , atEnd = "It is already last"
    }


{-| `announcement` is what to say after the last move, and nothing before the
first one. `onMove` is given the index a row is at and the index it moves to.
-}
type alias Config msg =
    { id : String
    , label : String
    , items : List (Item msg)
    , onMove : Int -> Int -> msg
    , announcement : Maybe String

    -- A way to add another, under the rows. Nothing where there is none.
    , add : Maybe { label : String, onAdd : msg }

    -- Dragging as well as the buttons. `state` is the drag in progress, which
    -- the caller holds; `onChange` is given the new state as it starts, moves
    -- and ends.
    , drag : Maybe { state : Maybe Dragging, onChange : Maybe Dragging -> msg }
    }


toHtml : Config msg -> Html msg
toHtml =
    toHtmlWithWords english


toHtmlWithWords : Words -> Config msg -> Html msg
toHtmlWithWords words config =
    div [ class "ui-reorderable" ]
        [ if List.isEmpty config.items then
            text ""

          else
            ol
                [ class "ui-reorderable__list", attribute "aria-label" config.label ]
                (withLandingSlot config (List.indexedMap (viewItem words config (List.length config.items)) config.items))
        , case config.add of
            Just add ->
                div [ class "ui-reorderable__add" ]
                    [ Button.toHtml
                        (Button.secondary add.label add.onAdd
                            |> Button.withIcon (Icon.solid "fa-plus")
                            |> Button.small
                            -- Named, so something outside can send the reader
                            -- here: a warning that the list is empty is about
                            -- this button and nothing else on the form.
                            |> Button.withId (config.id ++ "-add")
                        )
                    ]

            Nothing ->
                text ""

        -- The region is always here and always empty until something moves, so
        -- the announcement is a change to it rather than a new element, which
        -- is what a live region needs to notice.
        , div
            [ class "ui-visually-hidden"
            , attribute "role" "status"
            , attribute "aria-live" "polite"
            ]
            [ text (Maybe.withDefault "" config.announcement) ]
        ]


{-| The gap the dragged row would drop into, opened between the two rows it
would end up between. Nothing while the pointer is over the row's own place:
there is no move to show.
-}
withLandingSlot : Config msg -> List (Html msg) -> List (Html msg)
withLandingSlot config rows =
    case activeDrag config of
        Just drag ->
            if drag.over == drag.from then
                rows

            else
                let
                    landing =
                        if drag.over > drag.from then
                            drag.over + 1

                        else
                            drag.over

                    -- The slot takes the drop as well as showing it: it is
                    -- what the pointer is over by the time it is released, and
                    -- a gap that opens under the pointer and then swallows the
                    -- drop is worse than no gap at all.
                    -- Holding the drag where it is rather than doing nothing:
                    -- `preventDefaultOn` has to send something, and what it
                    -- must not send is a move.
                    hold =
                        case config.drag of
                            Just handlers ->
                                [ preventDefaultOn "dragover" (D.succeed ( handlers.onChange (Just drag), True )) ]

                            Nothing ->
                                []

                    slot =
                        li
                            (class "ui-reorderable__slot"
                                :: attribute "aria-hidden" "true"
                                :: preventDefaultOn "drop" (D.succeed ( config.onMove drag.from drag.over, True ))
                                :: hold
                            )
                            []
                in
                List.take landing rows ++ (slot :: List.drop landing rows)

        Nothing ->
            rows


{-| The drag in progress, if it is this list's. A page with several of these
passes the same state to all of them; only the one it started in answers.
-}
activeDrag : Config msg -> Maybe Dragging
activeDrag config =
    config.drag
        |> Maybe.andThen .state
        |> Maybe.andThen
            (\state ->
                if state.list == config.id then
                    Just state

                else
                    Nothing
            )


viewItem : Words -> Config msg -> Int -> Int -> Item msg -> Html msg
viewItem words listConfig count index (Item config) =
    let
        moveButton way icon label disabledReason target =
            Button.iconOnly icon label (listConfig.onMove index target)
                |> Button.small
                |> (case disabledReason of
                        Just reason ->
                            Button.withDisabledReason
                                { id = listConfig.id ++ "-" ++ String.fromInt index ++ "-" ++ way
                                , reason = reason
                                }

                        Nothing ->
                            identity
                   )
                |> Button.toHtml

        ( dragAttrs, grip ) =
            case listConfig.drag of
                Just drag ->
                    let
                        held =
                            activeDrag listConfig
                    in
                    ( [ classList [ ( "is-dragging", Maybe.map .from held == Just index ) ]

                      -- Both of these have to stop the browser's own handling
                      -- or nothing may be dropped here at all.
                      , preventDefaultOn "dragover"
                            (D.succeed
                                ( drag.onChange (Maybe.map (\state -> { state | over = index }) held)
                                , True
                                )
                            )
                      , preventDefaultOn "drop"
                            (D.succeed
                                ( case held of
                                    Just state ->
                                        listConfig.onMove state.from index

                                    Nothing ->
                                        drag.onChange Nothing
                                , True
                                )
                            )
                      ]
                    , [ span
                            [ class "ui-reorderable__grip"
                            , attribute "aria-hidden" "true"
                            , draggable "true"
                            , on "dragstart" (D.succeed (drag.onChange (Just { list = listConfig.id, from = index, over = index })))

                            -- After the drop, and after a drag abandoned
                            -- anywhere else, so nothing is left held.
                            , on "dragend" (D.succeed (drag.onChange Nothing))
                            ]
                            [ Icon.toHtml (Icon.solid "fa-bars") ]
                      ]
                    )

                Nothing ->
                    ( [], [] )
    in
    li (class "ui-reorderable__item" :: TestId.attribute (listConfig.id ++ "_item_" ++ config.key) :: dragAttrs)
        (grip
            ++ [ div [ class "ui-reorderable__content" ] config.content
               , div [ class "ui-reorderable__actions" ]
                    [ moveButton "up"
                        (Icon.solid "fa-arrow-up")
                        (words.moveUp config.label)
                        (if index == 0 then
                            Just words.atStart

                         else
                            Nothing
                        )
                        (index - 1)
                    , moveButton "down"
                        (Icon.solid "fa-arrow-down")
                        (words.moveDown config.label)
                        (if index >= count - 1 then
                            Just words.atEnd

                         else
                            Nothing
                        )
                        (index + 1)
                    ]
               ]
        )
