module Common.Plugins.PluginFilterTest exposing (pluginFilterTests)

import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Common.Data.AppState as AppState
import Common.Plugins.Plugin exposing (Plugin)
import Dict
import Expect
import Test exposing (Test, describe, test)
import Uuid exposing (Uuid)
import Version


uuid : String -> Uuid
uuid =
    Uuid.fromUuidString


excel : Uuid
excel =
    uuid "11111111-1111-4111-8111-111111111111"


ftp : Uuid
ftp =
    uuid "22222222-2222-4222-8222-222222222222"


plugin : Uuid -> String -> Plugin
plugin pluginUuid name =
    { uuid = pluginUuid
    , name = name
    , version = Version.create 1 0 0
    , description = ""
    , pluginApiVersion = Version.create 0 5 0
    , connectors =
        { documentActions = Nothing
        , knowledgeModelIntegrations = Nothing
        , projectActions = Nothing
        , projectImporters = Nothing
        , projectQuestionActions = Nothing
        , projectTabs = Nothing
        , settings = Nothing
        , userSettings = Nothing
        }
    }


state : List String -> List ( String, String ) -> { plugins : List Plugin, config : BootstrapConfig }
state disabled settings =
    let
        config =
            BootstrapConfig.default
    in
    { plugins = [ plugin excel "Excel", plugin ftp "FTP" ]
    , config = { config | disabledPlugins = disabled, pluginSettings = Dict.fromList settings }
    }


pluginFilterTests : Test
pluginFilterTests =
    describe "the plugins in force where the user is"
        [ test "every plugin that was loaded, when none is switched off" <|
            \_ ->
                AppState.getPlugins (state [] [])
                    |> List.map .name
                    |> Expect.equal [ "Excel", "FTP" ]
        , test "less those switched off" <|
            \_ ->
                AppState.getPlugins (state [ Uuid.toString ftp ] [])
                    |> List.map .name
                    |> Expect.equal [ "Excel" ]
        , test "a plugin switched off is not found by its uuid" <|
            \_ ->
                AppState.getPlugin (state [ Uuid.toString ftp ] []) ftp
                    |> Expect.equal Nothing
        , test "its settings are the ones in force" <|
            \_ ->
                AppState.getPluginSettings (state [] [ ( Uuid.toString excel, "{\"sheet\":\"A\"}" ) ]) excel
                    |> Expect.equal "{\"sheet\":\"A\"}"
        ]
