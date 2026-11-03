module Ui.ListItem exposing
    ( ListItem
    , item
    , list
    , tiles
    , withActions
    , withDescription
    , withDetail
    , withFragments
    , withHighlight
    , withIcon
    , withRowLink
    , withTestId
    , withTimestamp
    )

{-| The list view the product uses far more than it uses tables: an icon, a
title, a run of description fragments, a timestamp and a row of actions.

This is only the row: searching, filtering, sorting and pagination belong to
whatever holds the rows, and keeping them apart is what lets the same row
appear in a listing, a picker or a plain list.

Two things it does that a row is easily built without:

  - It is a real `ul` / `li`, so assistive technology announces how many items
    there are. Today it is a run of `div`s.
  - The `·` between fragments is generated with empty alternative text, so it is
    not read aloud. A middot announced between every fragment is noise.

The row is not itself a link. The title carries the link, which keeps the
actions and any links inside the fragments reachable rather than nested inside
a larger interactive element. `withRowLink` makes the whole row clickable
without changing that: the title's link is stretched over the row, and the
actions sit above it.

-}

import Html exposing (Html, div, li, span, text, time, ul)
import Html.Attributes exposing (attribute, class, classList)
import Ui.TestId as TestId


type ListItem msg
    = ListItem
        { title : Html msg
        , icon : Maybe (Html msg)
        , fragments : List (Html msg)
        , description : Maybe String
        , timestamp : Maybe { relative : String, absolute : String, machine : String }
        , actions : Maybe (Html msg)
        , detail : Maybe (Html msg)
        , highlighted : Bool
        , rowLink : Bool
        , testId : Maybe String
        }


item : Html msg -> ListItem msg
item title =
    ListItem
        { title = title
        , icon = Nothing
        , fragments = []
        , description = Nothing
        , timestamp = Nothing
        , actions = Nothing
        , detail = Nothing
        , highlighted = False
        , rowLink = False
        , testId = Nothing
        }


withIcon : Html msg -> ListItem msg -> ListItem msg
withIcon icon (ListItem config) =
    ListItem { config | icon = Just icon }


{-| Short pieces of metadata under the title — an identifier, a version, a
parent. They are separated visually by a middot that is not read aloud.
-}
withFragments : List (Html msg) -> ListItem msg -> ListItem msg
withFragments fragments (ListItem config) =
    ListItem { config | fragments = fragments }


{-| A sentence about the item, as the last fragment. It takes what width is left
on the fragments' line and is cut with an ellipsis, so a row stays its title and
that one line; only in a list too narrow to leave it any room does it move to a
line of its own, still cut short. It is the whole description to a screen
reader, and an empty one is left out.
-}
withDescription : String -> ListItem msg -> ListItem msg
withDescription description (ListItem config) =
    if String.isEmpty (String.trim description) then
        ListItem { config | description = Nothing }

    else
        ListItem { config | description = Just description }


{-| `relative` is what is shown ("3 days ago"), `absolute` what a pointer
reveals, `machine` the ISO value for the `datetime` attribute — so the date is
available to anything parsing the page rather than only to a reader.
-}
withTimestamp :
    { relative : String, absolute : String, machine : String }
    -> ListItem msg
    -> ListItem msg
withTimestamp timestamp (ListItem config) =
    ListItem { config | timestamp = Just timestamp }


{-| Typically a `Ui.Menu`. Taken as rendered Html because the menu holds state
and the row does not — a stateless row is what lets a listing, a picker and a
plain list share it.
-}
withActions : Html msg -> ListItem msg -> ListItem msg
withActions actions (ListItem config) =
    ListItem { config | actions = Just actions }


{-| A full-width area below the row, for anything that will not fit on the line.
-}
withDetail : Html msg -> ListItem msg -> ListItem msg
withDetail detail (ListItem config) =
    ListItem { config | detail = Just detail }


{-| The whole row follows the link in the title, for a row that has one place
to go.

It is still one link, named by the title: its hit area is stretched over the
row rather than the row being wrapped in an `a`, so a screen reader does not
read the icon, fragments and timestamp as the link's name, and actions or links
in the fragments stay separate targets above it. The focus ring is drawn on the
row, where the link now is.

The title is cut to one line with an ellipsis and is not underlined: the row is
a way into the item, and its hover ground already says the whole of it is the
target. The full title is still the link's name.

-}
withRowLink : ListItem msg -> ListItem msg
withRowLink (ListItem config) =
    ListItem { config | rowLink = True }


withHighlight : Bool -> ListItem msg -> ListItem msg
withHighlight highlighted (ListItem config) =
    ListItem { config | highlighted = highlighted }


{-| The test hook on the row itself (`data-cy`), so a test can find the row
for one item without matching its title.
-}
withTestId : String -> ListItem msg -> ListItem msg
withTestId testId (ListItem config) =
    ListItem { config | testId = Just testId }


list : List (ListItem msg) -> Html msg
list items =
    ul [ class "ui-list" ] (List.map toHtml items)


{-| The same rows, each on a quiet ground of its own (the frame's, as the side
navigation has) with a gap between them instead of a rule, and no hover — for
rows to read or act in rather than to follow: the files of an import, the
members in the share dialog, where rules between rows read as more of the rules
between the dialog's sections and the list stops looking like one thing.
-}
tiles : List (ListItem msg) -> Html msg
tiles items =
    ul [ class "ui-list ui-list--tiles" ] (List.map toHtml items)


toHtml : ListItem msg -> Html msg
toHtml (ListItem config) =
    li
        (classList
            [ ( "ui-list__item", True )
            , ( "is-highlighted", config.highlighted )
            , ( "ui-list__item--row-link", config.rowLink )
            ]
            :: TestId.attributes config.testId
        )
        [ div [ class "ui-list__row" ]
            [ case config.icon of
                Just icon ->
                    div [ class "ui-list__icon" ] [ icon ]

                Nothing ->
                    text ""
            , div [ class "ui-list__content" ]
                [ div [ class "ui-list__title", TestId.attribute "list-item_title" ] [ config.title ]
                , let
                    fragments =
                        List.map (\fragment -> span [ class "ui-list__fragment" ] [ fragment ]) config.fragments

                    description =
                        case config.description of
                            Just value ->
                                [ span [ class "ui-list__fragment ui-list__fragment--description", TestId.attribute "list-item_description" ] [ text value ] ]

                            Nothing ->
                                []
                  in
                  if List.isEmpty fragments && List.isEmpty description then
                    text ""

                  else
                    div [ class "ui-list__fragments" ] (fragments ++ description)
                ]
            , case config.timestamp of
                Just stamp ->
                    time
                        [ class "ui-list__timestamp"
                        , attribute "datetime" stamp.machine
                        , attribute "title" stamp.absolute
                        ]
                        [ text stamp.relative ]

                Nothing ->
                    text ""
            , case config.actions of
                Just actions ->
                    div [ class "ui-list__actions" ] [ actions ]

                Nothing ->
                    text ""
            ]
        , case config.detail of
            Just detail ->
                div [ class "ui-list__detail" ] [ detail ]

            Nothing ->
                text ""
        ]
