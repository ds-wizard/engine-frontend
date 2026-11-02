module Common.Components.EntityCard exposing (Card, slot, view)

{-| The one thing a dialog is about — the user being deleted, the member being
removed, the project, the data source — as a framed box set apart from the
question over it: its icon at the size a list row has it, and beside it the name
with its badges (a role, "private") and, close under it, its id as code and a
muted line (an e-mail, a description, cut to two lines).

Every dialog that asks about one thing shows it this way, so the reader
recognizes it by the same picture they clicked it in, and the dialogs look alike
whatever they are about.

-}

import Html exposing (Html)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Ui.Badge as Badge exposing (Badge)
import Ui.ItemIcon as ItemIcon exposing (ItemIcon)
import Ui.Layout as Layout
import Ui.Text as Text


type alias Card msg =
    { icon : ItemIcon
    , name : String
    , badges : List (Badge msg)
    , code : Maybe String
    , detail : Maybe String
    }


view : Card msg -> Html msg
view card =
    Html.div [ class "c-entity-card" ]
        [ ItemIcon.toHtml card.icon
        , Html.div [ class "c-entity-card__text" ]
            (Layout.inline (Text.label card.name :: List.map Badge.toHtml card.badges)
                :: Maybe.values
                    [ Maybe.map Text.code card.code
                    , Maybe.andThen nonEmpty card.detail |> Maybe.map Text.muted
                    ]
            )
        ]


{-| The card of the thing a dialog is asking about, in an element that is there
whether or not a thing is chosen: a dialog must not gain or lose children while
it is on the page.
-}
slot : Maybe (Card msg) -> Html msg
slot mbCard =
    Html.div [] (List.filterMap (Maybe.map view) [ mbCard ])


nonEmpty : String -> Maybe String
nonEmpty value =
    if String.isEmpty (String.trim value) then
        Nothing

    else
        Just value
