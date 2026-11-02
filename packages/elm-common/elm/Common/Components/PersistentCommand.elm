module Common.Components.PersistentCommand exposing (badge)

{-| What state a persistent command is in, as a badge.

The badge's words are the state, so the color repeats what the text already
says rather than carrying it alone (1.4.1): "Error" is red and also says
"Error".

-}

import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Ui.Badge as Badge exposing (Badge)


badge : WithLocale a -> PersistentCommandState -> Badge msg
badge appState state =
    case state of
        PersistentCommandState.New ->
            Badge.info (gettext "New" appState.locale)

        PersistentCommandState.Done ->
            Badge.success (gettext "Done" appState.locale)

        PersistentCommandState.Error ->
            Badge.danger (gettext "Error" appState.locale)

        PersistentCommandState.Ignore ->
            Badge.neutral (gettext "Ignore" appState.locale)
