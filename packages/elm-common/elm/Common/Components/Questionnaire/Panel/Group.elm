module Common.Components.Questionnaire.Panel.Group exposing
    ( Msg(..)
    , Row
    , view
    )

{-| Questions grouped by their chapter, each a button that goes to it: what the
unanswered questions, the TODOs, the warnings and the comments overview all are.

The panels differ in their mark, their empty state and what they put at the end of
a row; the shape is this one, written once.

**Its structure is stated.** A chapter is an `h3` and its
questions a list that points at it, so a screen reader hears "list, 4 items" under
the chapter the questions belong to. A row is a real button that looks like a
link — what it does is go somewhere in the questionnaire, which is what a link
looks like, but it goes there by scrolling and focusing rather than by loading
an address, and a link with no address is reachable by nobody but a mouse. What a row carries at its end (a count) says in
words what it is; nothing here is said by color or position alone.

-}

import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Components.Questionnaire.Path as Path
import Gettext exposing (Locale)
import Html exposing (Html, button, div, h3, li, span, text, ul)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Ui.EmptyState as EmptyState
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId


type Msg
    = Selected String


{-| One question in a panel: where it is, what it says, and what it shows at the
end of its line.
-}
type alias Row =
    { path : String
    , title : String
    , end : List (Html Msg)
    }


{-| `rows` are in the questionnaire's own order; they are grouped here, keeping
the order each chapter was first seen in.
-}
view :
    { locale : Locale
    , icon : Icon
    , empty : { title : String, description : String }
    , items : List { chapter : Chapter, row : Row }
    }
    -> Html Msg
view config =
    case group config.items of
        [] ->
            div [ class "c-questionnaire-panel c-questionnaire-panel--empty" ]
                [ EmptyState.toHtml
                    (EmptyState.emptyState config.empty
                        |> EmptyState.withIllustration (Icon.toHtml config.icon)
                        |> EmptyState.withHeadingLevel 3
                    )
                ]

        groups ->
            div [ class "c-questionnaire-panel" ]
                (List.map (viewGroup config.icon) groups)


viewGroup : Icon -> ( Chapter, List Row ) -> Html Msg
viewGroup icon ( chapter, rows ) =
    let
        headingId =
            "questionnaire-panel-" ++ Path.toFieldId chapter.uuid
    in
    div [ class "c-questionnaire-panel__group" ]
        [ h3 [ class "c-questionnaire-panel__chapter", id headingId ] [ text chapter.title ]
        , ul [ class "c-questionnaire-panel__list", attribute "aria-labelledby" headingId ]
            (List.map (viewRow icon) rows)
        ]


viewRow : Icon -> Row -> Html Msg
viewRow icon row =
    li [ class "c-questionnaire-panel__row", TestId.attribute "questionnaire-panel_row" ]
        -- The mark is the panel's, not the question's: it says the same thing
        -- on every row, so it stays outside the link rather than joining its
        -- name and its underline, as the wizard had it.
        (span [ class "c-questionnaire-panel__icon", attribute "aria-hidden" "true" ] [ Icon.toHtml icon ]
            :: button
                [ type_ "button"
                , class "ui-link c-questionnaire-panel__link"
                , onClick (Selected row.path)

                -- By the question's path, which is what the row goes to.
                , TestId.attribute ("questionnaire-panel_item_" ++ row.path)
                ]
                [ text row.title ]
            :: row.end
        )


{-| The rows by chapter, each chapter in the order it first appears and its rows
in the order they came.
-}
group : List { chapter : Chapter, row : Row } -> List ( Chapter, List Row )
group items =
    items
        |> List.foldl
            (\item acc ->
                case List.findIndex (\( chapter, _ ) -> chapter.uuid == item.chapter.uuid) acc of
                    Just index ->
                        List.updateAt index (\( chapter, rows ) -> ( chapter, item.row :: rows )) acc

                    Nothing ->
                        acc ++ [ ( item.chapter, [ item.row ] ) ]
            )
            []
        |> List.map (\( chapter, rows ) -> ( chapter, List.reverse rows ))
