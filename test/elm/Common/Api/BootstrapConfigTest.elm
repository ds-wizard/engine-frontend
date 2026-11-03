module Common.Api.BootstrapConfigTest exposing (bootstrapConfigTests)

import Common.Api.Models.BootstrapConfig as BootstrapConfig
import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig exposing (RegistryConfig(..))
import Expect
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Test exposing (Test, describe, test)


bootstrap : List ( String, E.Value ) -> E.Value
bootstrap overrides =
    let
        fields =
            [ ( "features", E.object [ ( "aiAssistantEnabled", E.bool False ), ( "toursEnabled", E.bool True ) ] )
            , ( "authentication"
              , E.object
                    [ ( "internal"
                      , E.object
                            [ ( "registrationEnabled", E.bool False )
                            , ( "nonAdminLoginEnabled", E.bool True )
                            , ( "sessionExpiration", E.int 336 )
                            , ( "userEmailLinkExpiration", E.int 24 )
                            , ( "twoFactorAuth", E.object [ ( "enabled", E.bool False ), ( "codeLength", E.int 6 ), ( "expiration", E.int 600 ) ] )
                            ]
                      )
                    , ( "openId", E.list identity [] )
                    ]
              )
            , ( "loginScreen", E.object [ ( "loginInfo", E.null ), ( "loginInfoSidebar", E.null ), ( "announcements", E.list identity [] ) ] )
            , ( "registry", E.object [ ( "enabled", E.bool False ), ( "url", E.null ) ] )
            , ( "lookAndFeel", E.object [ ( "appTitle", E.null ), ( "logoUrl", E.null ), ( "primaryColor", E.null ) ] )
            , ( "users", E.object [ ( "affiliations", E.list E.string [ "Smoke University" ] ) ] )
            , ( "privacy", E.object [ ( "privacyUrl", E.null ), ( "termsOfServiceUrl", E.null ) ] )
            , ( "plugins", E.list identity [] )
            , ( "cloud", E.object [ ( "enabled", E.bool False ) ] )
            , ( "tours", E.list identity [] )
            , ( "user", E.null )
            , ( "dashboardAndMenu", E.object [ ( "customMenuLinks", E.list identity [] ), ( "announcements", E.list identity [] ) ] )
            , ( "support", E.object [ ( "supportEmail", E.null ), ( "supportSiteName", E.null ), ( "supportSiteUrl", E.null ), ( "supportSiteIcon", E.null ) ] )
            , ( "submission", E.object [ ( "enabled", E.bool True ) ] )
            , ( "pluginSettings", E.object [] )
            ]

        names =
            List.map Tuple.first overrides
    in
    E.object (List.filter (\( name, _ ) -> List.notMember name names) fields ++ overrides)


without : String -> E.Value
without name =
    D.decodeValue (D.keyValuePairs D.value) (bootstrap [])
        |> Result.withDefault []
        |> List.filter (\( key, _ ) -> key /= name)
        |> E.object


workspace : List ( String, E.Value ) -> E.Value
workspace overrides =
    E.object
        ([ ( "logo", E.null )
         , ( "primaryColor", E.null )
         , ( "dashboardAndMenu", E.object [ ( "customMenuLinks", E.list identity [] ), ( "announcements", E.list identity [] ) ] )
         , ( "projects"
           , E.object
                [ ( "projectVisibility", E.object [ ( "enabled", E.bool True ), ( "defaultValue", E.string "PrivateProjectVisibility" ) ] )
                , ( "projectSharing", E.object [ ( "enabled", E.bool True ), ( "defaultValue", E.string "RestrictedProjectSharing" ), ( "anonymousEnabled", E.bool False ) ] )
                , ( "projectCreation", E.string "TemplateAndCustomProjectCreation" )
                , ( "summaryReport", E.object [ ( "enabled", E.bool True ) ] )
                , ( "projectTagging", E.object [ ( "enabled", E.bool True ), ( "tags", E.list E.string [] ) ] )
                ]
           )
         , ( "support", E.object [ ( "supportEmail", E.null ), ( "supportSiteName", E.null ), ( "supportSiteUrl", E.null ), ( "supportSiteIcon", E.null ) ] )
         , ( "submission", E.object [ ( "enabled", E.bool True ), ( "services", E.list identity [] ) ] )
         ]
            ++ overrides
        )


bootstrapConfigTests : Test
bootstrapConfigTests =
    describe "Common.Api.Models.BootstrapConfig"
        [ describe "decoder"
            [ test "reads the authentication section from `internal`" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (bootstrap [])
                        |> Result.map (\config -> ( config.authentication.registrationEnabled, config.authentication.nonAdminLoginEnabled ))
                        |> Expect.equal (Ok ( False, True ))
            , test "reads the registry that is there" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (bootstrap [])
                        |> Result.map .registry
                        |> Expect.equal (Ok RegistryDisabled)
            , test "a server with no registry section has the registry on" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "registry")
                        |> Result.map (RegistryConfig.isEnabled << .registry)
                        |> Expect.equal (Ok True)
            , test "a registry that cannot be read is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "registry", E.object [] ) ])
                        |> Expect.err
            , test "a missing users section is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "users")
                        |> Expect.err
            , test "a missing privacy section is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "privacy")
                        |> Expect.err
            , test "reads whether documents can be submitted" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (bootstrap [])
                        |> Result.map (.enabled << .submission)
                        |> Expect.equal (Ok True)
            , test "a missing dashboard and menu section is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "dashboardAndMenu")
                        |> Expect.err
            , test "missing plugin settings are an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "pluginSettings")
                        |> Expect.err
            , test "a missing submission section is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "submission")
                        |> Expect.err
            , test "a user that cannot be read is an error, not nobody signed in" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "user", E.object [] ) ])
                        |> Expect.err
            ]
        , describe "workspaceDecoder"
            [ test "reads what the workspace says about plugins" <|
                \_ ->
                    D.decodeValue BootstrapConfig.workspaceDecoder
                        (workspace [ ( "disabledPlugins", E.list E.string [ "p" ] ), ( "pluginSettings", E.object [] ) ])
                        |> Result.map .disabledPlugins
                        |> Expect.equal (Ok [ "p" ])
            , test "missing plugin fields are an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.workspaceDecoder (workspace [])
                        |> Expect.err
            , test "reads the logo as the server names it" <|
                \_ ->
                    D.decodeValue BootstrapConfig.workspaceDecoder
                        (workspace [ ( "logo", E.string "https://example.com/logo.png" ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        |> Result.map .logoUrl
                        |> Expect.equal (Ok (Just "https://example.com/logo.png"))
            ]
        , describe "custom menu links"
            [ test "the organization's, where the server gives them, are back once a workspace is left" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            config
                                |> BootstrapConfig.withWorkspace workspaceConfig
                                |> BootstrapConfig.withoutWorkspace
                                |> .dashboardAndMenu
                                |> .customMenuLinks
                                |> List.map .title
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "dashboardAndMenu", menu "Organization link" ) ]))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "dashboardAndMenu", menu "Workspace link" ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok [ "Organization link" ])
            , test "a workspace's are its own while in it" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            BootstrapConfig.withWorkspace workspaceConfig config
                                |> .dashboardAndMenu
                                |> .customMenuLinks
                                |> List.map .title
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "dashboardAndMenu", menu "Organization link" ) ]))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "dashboardAndMenu", menu "Workspace link" ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok [ "Workspace link" ])
            ]
        , describe "submission"
            [ test "the organization's is back once a workspace is left" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            BootstrapConfig.withWorkspace workspaceConfig config
                                |> BootstrapConfig.withoutWorkspace
                                |> .submission
                                |> .enabled
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap []))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "submission", E.object [ ( "enabled", E.bool False ) ] ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok True)
            , test "a workspace's is its own while in it" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            BootstrapConfig.withWorkspace workspaceConfig config
                                |> .submission
                                |> .enabled
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap []))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "submission", E.object [ ( "enabled", E.bool False ) ] ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok False)
            ]
        , describe "support"
            [ test "the organization's, where the server gives it, is back once a workspace is left" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            config
                                |> BootstrapConfig.withWorkspace workspaceConfig
                                |> BootstrapConfig.withoutWorkspace
                                |> .support
                                |> .supportEmail
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "support", support "organization@example.com" ) ]))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "support", support "workspace@example.com" ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok (Just "organization@example.com"))
            , test "a workspace's is its own while in it" <|
                \_ ->
                    Result.map2
                        (\config workspaceConfig ->
                            BootstrapConfig.withWorkspace workspaceConfig config
                                |> .support
                                |> .supportEmail
                        )
                        (D.decodeValue BootstrapConfig.decoder (bootstrap [ ( "support", support "organization@example.com" ) ]))
                        (D.decodeValue BootstrapConfig.workspaceDecoder
                            (workspace [ ( "support", support "workspace@example.com" ), ( "disabledPlugins", E.list E.string [] ), ( "pluginSettings", E.object [] ) ])
                        )
                        |> Expect.equal (Ok (Just "workspace@example.com"))
            , test "a missing support section is an error" <|
                \_ ->
                    D.decodeValue BootstrapConfig.decoder (without "support")
                        |> Expect.err
            ]
        ]


support : String -> E.Value
support email =
    E.object
        [ ( "supportEmail", E.string email )
        , ( "supportSiteName", E.null )
        , ( "supportSiteUrl", E.null )
        , ( "supportSiteIcon", E.null )
        ]


menu : String -> E.Value
menu title =
    E.object
        [ ( "customMenuLinks"
          , E.list identity
                [ E.object [ ( "icon", E.string "fas fa-link" ), ( "title", E.string title ), ( "url", E.string "https://example.com" ), ( "newWindow", E.bool False ) ] ]
          )
        , ( "announcements", E.list identity [] )
        ]
