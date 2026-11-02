module Common.Components.RowBadge exposing (badge, owner)

{-| The badge an application adds to a row (`Common.Data.RowPolicy.RowBadge`):
purple, because it says whose a row is rather than its state, with its icon if
it has one.
-}

import Common.Data.RowPolicy exposing (RowBadge)
import Ui.Badge as Badge exposing (Badge)
import Ui.Icon as Icon
import Uuid exposing (Uuid)


badge : RowBadge -> Badge msg
badge rowBadge =
    let
        withIcon =
            case rowBadge.icon of
                Just icon ->
                    Badge.withIcon (Icon.solid icon)

                Nothing ->
                    identity
    in
    Badge.private rowBadge.label
        |> withIcon
        |> Badge.withTestId rowBadge.testId


{-| The badge saying whose a thing is, where it is shown outside its listing
(`appState.ownerBadge`), if the application has one for it: none or one.
-}
owner : { a | ownerBadge : Maybe Uuid -> Maybe RowBadge } -> Maybe Uuid -> List (Badge msg)
owner appState workspaceUuid =
    case appState.ownerBadge workspaceUuid of
        Just rowBadge ->
            [ badge rowBadge ]

        Nothing ->
            []
