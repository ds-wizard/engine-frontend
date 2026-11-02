module Common.Api.PluginSettings exposing
    ( deletePluginValues
    , getPluginStates
    , getPluginValues
    , putPluginStates
    , putPluginValues
    )

{-| The settings of plugins: `/plugin-settings`. Whose they are is the
application's to say (`Common.Api.Params`), as for the settings sections.
-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.PluginSettings as PluginSettings exposing (PluginState)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Uuid exposing (Uuid)


getPluginStates : Context ctx -> ToMsg (List PluginState) msg -> Cmd msg
getPluginStates appState =
    Request.get (Context.toServerInfo appState)
        (Params.apply appState "GET" "/plugin-settings")
        (D.list PluginSettings.stateDecoder)


putPluginStates : Context ctx -> List PluginState -> ToMsg () msg -> Cmd msg
putPluginStates appState states =
    Request.putWhatever (Context.toServerInfo appState)
        (Params.apply appState "PUT" "/plugin-settings")
        (PluginSettings.encodeStates states)


{-| The response as it comes; `PluginSettings.valuesFromResponse` reads it.
-}
getPluginValues : Context ctx -> Uuid -> ToMsg String msg -> Cmd msg
getPluginValues appState pluginUuid =
    Request.getString (Context.toServerInfo appState)
        (Params.apply appState "GET" (valuesUrl pluginUuid))


putPluginValues : Context ctx -> Uuid -> String -> ToMsg () msg -> Cmd msg
putPluginValues appState pluginUuid =
    Request.putString (Context.toServerInfo appState)
        (Params.apply appState "PUT" (valuesUrl pluginUuid))
        "application/json"


{-| Give up settings of one's own for a plugin, back to the defaults.
-}
deletePluginValues : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deletePluginValues appState pluginUuid =
    Request.delete (Context.toServerInfo appState)
        (Params.apply appState "DELETE" (valuesUrl pluginUuid))


valuesUrl : Uuid -> String
valuesUrl pluginUuid =
    "/plugin-settings/" ++ Uuid.toString pluginUuid
