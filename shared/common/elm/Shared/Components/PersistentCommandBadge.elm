module Shared.Components.PersistentCommandBadge exposing (badge)

import Html exposing (Html, text)
import Shared.Components.Badge as Badge
import Shared.Data.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)


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
