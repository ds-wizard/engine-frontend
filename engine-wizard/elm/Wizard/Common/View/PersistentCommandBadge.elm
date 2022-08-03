module Wizard.Common.View.PersistentCommandBadge exposing (view)

import Html exposing (Html, text)
import Shared.Components.Badge as Badge
import Shared.Data.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)


view : { a | state : PersistentCommandState } -> Html msg
view persistentCommand =
    case persistentCommand.state of
        PersistentCommandState.New ->
            Badge.info [] [ text "New" ]

        PersistentCommandState.Done ->
            Badge.success [] [ text "Done" ]

        PersistentCommandState.Error ->
            Badge.danger [] [ text "Error" ]

        PersistentCommandState.Ignore ->
            Badge.secondary [] [ text "Ignore" ]
