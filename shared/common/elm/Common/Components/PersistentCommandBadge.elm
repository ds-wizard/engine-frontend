module Common.Components.PersistentCommandBadge exposing (badge)

import Common.Components.Badge as Badge
import Common.Data.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Html exposing (Html, text)


badge : { a | state : PersistentCommandState } -> Html msg
badge persistentCommand =
    case persistentCommand.state of
        PersistentCommandState.New ->
            Badge.info [] [ text "New" ]

        PersistentCommandState.Done ->
            Badge.success [] [ text "Done" ]

        PersistentCommandState.Error ->
            Badge.danger [] [ text "Error" ]

        PersistentCommandState.Ignore ->
            Badge.secondary [] [ text "Ignore" ]
