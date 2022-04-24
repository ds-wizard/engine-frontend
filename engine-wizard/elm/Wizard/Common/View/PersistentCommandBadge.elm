module Wizard.Common.View.PersistentCommandBadge exposing (view)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Data.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)


view : { a | state : PersistentCommandState } -> Html msg
view persistentCommand =
    case persistentCommand.state of
        PersistentCommandState.New ->
            span [ class "badge badge-info" ] [ text "New" ]

        PersistentCommandState.Done ->
            span [ class "badge badge-success" ] [ text "Done" ]

        PersistentCommandState.Error ->
            span [ class "badge badge-danger" ] [ text "Error" ]

        PersistentCommandState.Ignore ->
            span [ class "badge badge-secondary" ] [ text "Ignore" ]
