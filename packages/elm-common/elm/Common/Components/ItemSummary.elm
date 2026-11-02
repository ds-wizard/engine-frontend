module Common.Components.ItemSummary exposing
    ( Item
    , view
    )

{-| One thing from the API, said in a row: the round icon made from its name,
the name, the id that tells two similar names apart, and what the thing is.

It is the row a `Common.Components.TypeHint` offers and then shows as its
value, and it is here rather than in `TypeHint` because it is not a part of a
combobox. The same row is how a page shows a thing that was chosen once and is
now only to be read — the knowledge model a project runs on — and a page doing
that has no business importing a form control to get it. What is chosen and
what is shown afterwards are then the same picture, drawn by one function.

    ItemSummary.view
        { label = package.name
        , code = Just package.id
        , description = Just package.description
        , icon = Nothing
        , badges = []
        }

**The row is not a control.** It has no border, no hover and no tab stop of its
own; whatever it sits in decides that. Inside a combobox's trigger it is the
value of a field, and inside a `Ui.Card` it is information.

**Everything is inside one element**, so it is read as one thing rather than as
four. The id is a neutral badge: a fixed piece of data rather than a sentence,
labeled as the badges beside it are. The other badges follow it on the same
line, so the id and whose the thing is — which tells apart two things with the
same id — read as one set of labels. The description is muted, so the eye goes
down the names.

**Spans throughout, laid out by the stylesheet.** The combobox draws its chosen
value with `view` inside the trigger's `button`, which may hold phrasing content
only. The way to the thing's own page is therefore never in the row: a link
inside a button is two controls in one, and neither works. A page that shows the
row as information puts the link beside it.

-}

import Html exposing (Html, span)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Ui.Badge as Badge exposing (Badge)
import Ui.ItemIcon as ItemIcon exposing (ItemIcon)
import Ui.Text as Text


{-| What the row says. `code` is the identifier — a package id, a version — and
`description` the sentence about the thing; either can be left out. `icon` is
the thing's own picture — a person's avatar (`UserAvatar.user`), so a person
looks the same in a picker as in a list; without one, the round icon is made
from the name. `badges` follow the id on its line — whose the thing is, when
two things can have the same id; they do nothing, so they cannot be a control
inside the combobox's trigger.
-}
type alias Item =
    { label : String
    , code : Maybe String
    , description : Maybe String
    , icon : Maybe ItemIcon
    , badges : List (Badge Never)
    }


{-| The row, all of it text.
-}
view : Item -> Html msg
view item =
    let
        codeLine =
            case Maybe.toList (Maybe.map idBadge item.code) ++ List.map (Html.map never << Badge.toHtml) item.badges of
                [] ->
                    Nothing

                content ->
                    Just (span [ class "c-item-summary__code" ] content)

        lines =
            Text.label item.label
                :: Maybe.values
                    [ codeLine
                    , Maybe.map Text.muted item.description
                    ]
    in
    span [ class "c-item-summary" ]
        [ ItemIcon.toHtml (Maybe.withDefault (ItemIcon.view item.label) item.icon)
        , span [ class "c-item-summary__lines" ] lines
        ]


{-| In a span of its own, so a long id is cut short rather than the row pushed
open.
-}
idBadge : String -> Html msg
idBadge code =
    span [ class "c-item-summary__id" ] [ Badge.toHtml (Badge.neutral code) ]
