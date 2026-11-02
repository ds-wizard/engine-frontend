module Common.Api.Models.PluginSettings exposing
    ( PluginState
    , PluginValues
    , encodeStates
    , stateDecoder
    , valuesFromResponse
    )

{-| The settings of plugins, as `/plugin-settings` reads and writes them.

A plugin is switched on or off (`PluginState`) and has settings of its own,
which only the plugin understands (`PluginValues`). Like a settings section,
both say what applies about overriding and leave out what does not:

  - where plugins are made available, `workspaceOverrideAllowed` says whether
    the settings may be replaced below;
  - where they are used, `overrideAllowed` says whether they may be replaced
    there, and `overridden` whether they have been.

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


type alias PluginState =
    { pluginUuid : Uuid
    , enabled : Bool
    , workspaceOverrideAllowed : Maybe Bool
    , overrideAllowed : Maybe Bool
    , overridden : Maybe Bool
    }


stateDecoder : Decoder PluginState
stateDecoder =
    D.succeed PluginState
        |> D.required "pluginUuid" Uuid.decoder
        |> D.required "enabled" D.bool
        |> D.optional "workspaceOverrideAllowed" (D.nullable D.bool) Nothing
        |> D.optional "overrideAllowed" (D.nullable D.bool) Nothing
        |> D.optional "overridden" (D.nullable D.bool) Nothing


{-| What is written back: for each plugin whether it is on and, where that can
be said, whether its settings may be overridden.
-}
encodeStates : List PluginState -> E.Value
encodeStates states =
    let
        encodeState state =
            E.object
                (( "enabled", E.bool state.enabled )
                    :: Maybe.unwrap [] (\allowed -> [ ( "workspaceOverrideAllowed", E.bool allowed ) ]) state.workspaceOverrideAllowed
                )
    in
    E.object (List.map (\state -> ( Uuid.toString state.pluginUuid, encodeState state )) states)


type alias PluginValues =
    { values : String
    , overrideAllowed : Maybe Bool
    , overridden : Maybe Bool
    }


{-| A plugin's settings as they come back: on their own, or in the three
fields that say whose they are. The settings themselves are kept as the text
the plugin's element takes.
-}
valuesFromResponse : String -> PluginValues
valuesFromResponse response =
    let
        wrapped =
            D.succeed PluginValues
                |> D.required "values" (D.map valueToString D.value)
                |> D.required "overrideAllowed" (D.nullable D.bool)
                |> D.required "overridden" (D.nullable D.bool)

        valueToString value =
            if value == E.null then
                ""

            else
                E.encode 0 value
    in
    D.decodeString wrapped response
        |> Result.withDefault { values = response, overrideAllowed = Nothing, overridden = Nothing }
