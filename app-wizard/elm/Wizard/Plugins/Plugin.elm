module Wizard.Plugins.Plugin exposing
    ( ActionWithIcon
    , Connectors
    , DocumentActionConnector
    , Plugin
    , ProjectActionConnector
    , ProjectImporterConnector
    , ProjectQuestionActionConnector
    , ProjectQuestionActionConnectorType(..)
    , ProjectTabConnector
    , SettingsConnector
    , decoder
    , filterByDtFormats
    , filterByDtPatterns
    , filterByKmPatterns
    , isApiVersionSupported
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Plugins.IdentifierPattern as IdentifierPattern exposing (IdentifierPattern)
import Wizard.Plugins.PluginElement as PluginElement exposing (PluginElement)


pluginApiVersion : Version
pluginApiVersion =
    Version.create 0 1 0


isApiVersionSupported : { a | pluginApiVersion : Version, version : Version } -> Bool
isApiVersionSupported plugin =
    let
        pluginMajor =
            Version.getMajor plugin.pluginApiVersion

        supportedMajor =
            Version.getMajor pluginApiVersion

        cmp =
            Version.compare plugin.pluginApiVersion pluginApiVersion
    in
    supportedMajor == pluginMajor && (cmp == LT || cmp == EQ)


type alias Plugin =
    { uuid : Uuid
    , name : String
    , version : Version
    , description : String
    , pluginApiVersion : Version
    , connectors : Connectors
    }


type alias Connectors =
    { documentActions : Maybe (List DocumentActionConnector)
    , projectActions : Maybe (List ProjectActionConnector)
    , projectImporters : Maybe (List ProjectImporterConnector)
    , projectQuestionActions : Maybe (List ProjectQuestionActionConnector)
    , projectTabs : Maybe (List ProjectTabConnector)
    , settings : Maybe SettingsConnector
    , userSettings : Maybe SettingsConnector
    }


type alias ActionWithIcon =
    { icon : String
    , name : String
    }


type alias DocumentActionConnector =
    { action : ActionWithIcon
    , element : PluginElement
    , dtPatterns : Maybe (List IdentifierPattern)
    , dtFormats : Maybe (List Uuid)
    }


type alias ProjectActionConnector =
    { name : String
    , element : PluginElement
    , kmPatterns : Maybe (List IdentifierPattern)
    }


type alias ProjectImporterConnector =
    { name : String
    , url : String
    , element : PluginElement
    , kmPatterns : Maybe (List IdentifierPattern)
    }


type alias ProjectQuestionActionConnector =
    { action : ActionWithIcon
    , type_ : ProjectQuestionActionConnectorType
    , element : PluginElement
    , kmPatterns : Maybe (List IdentifierPattern)
    }


type ProjectQuestionActionConnectorType
    = ModalProjectQuestionAction
    | SidebarProjectQuestionAction


type alias ProjectTabConnector =
    { tab : ActionWithIcon
    , url : String
    , element : PluginElement
    , kmPatterns : Maybe (List IdentifierPattern)
    }


type alias SettingsConnector =
    { element : PluginElement
    }


filterByKmPatterns : String -> List ( a, { b | kmPatterns : Maybe (List IdentifierPattern) } ) -> List ( a, { b | kmPatterns : Maybe (List IdentifierPattern) } )
filterByKmPatterns identifier =
    List.filter (matchesIdentifierPattern identifier << .kmPatterns << Tuple.second)


filterByDtPatterns : String -> List ( a, { b | dtPatterns : Maybe (List IdentifierPattern) } ) -> List ( a, { b | dtPatterns : Maybe (List IdentifierPattern) } )
filterByDtPatterns identifier =
    List.filter (matchesIdentifierPattern identifier << .dtPatterns << Tuple.second)


filterByDtFormats : Uuid -> List ( a, { b | dtFormats : Maybe (List Uuid) } ) -> List ( a, { b | dtFormats : Maybe (List Uuid) } )
filterByDtFormats formatUuid =
    List.filter (matchesUuid formatUuid << .dtFormats << Tuple.second)


matchesIdentifierPattern : String -> Maybe (List IdentifierPattern) -> Bool
matchesIdentifierPattern identifierStr mbPatterns =
    case mbPatterns of
        Nothing ->
            True

        Just patterns ->
            List.any (\pattern -> IdentifierPattern.matches pattern identifierStr) patterns


matchesUuid : Uuid -> Maybe (List Uuid) -> Bool
matchesUuid uuid mbUuidList =
    case mbUuidList of
        Nothing ->
            True

        Just uuids ->
            List.member uuid uuids


decoder : Decoder Plugin
decoder =
    D.succeed Plugin
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "pluginApiVersion" Version.decoder
        |> D.required "connectors" connectorsDecoder


connectorsDecoder : Decoder Connectors
connectorsDecoder =
    D.succeed Connectors
        |> D.optional "documentActions" (D.maybe (D.list documentActionConnectorDecoder)) Nothing
        |> D.optional "projectActions" (D.maybe (D.list projectActionConnectorDecoder)) Nothing
        |> D.optional "projectImporters" (D.maybe (D.list projectImporterConnectorDecoder)) Nothing
        |> D.optional "projectQuestionActions" (D.maybe (D.list projectQuestionActionConnectorDecoder)) Nothing
        |> D.optional "projectTabs" (D.maybe (D.list projectTabConnectorDecoder)) Nothing
        |> D.optional "settings" (D.maybe settingsConnectorDecoder) Nothing
        |> D.optional "userSettings" (D.maybe settingsConnectorDecoder) Nothing


actionWithIconDecoder : Decoder ActionWithIcon
actionWithIconDecoder =
    D.succeed ActionWithIcon
        |> D.required "icon" D.string
        |> D.required "name" D.string


documentActionConnectorDecoder : Decoder DocumentActionConnector
documentActionConnectorDecoder =
    D.succeed DocumentActionConnector
        |> D.required "action" actionWithIconDecoder
        |> D.required "element" PluginElement.decoder
        |> D.required "dtPatterns" (D.maybe (D.list IdentifierPattern.decoder))
        |> D.required "dtFormats" (D.maybe (D.list Uuid.decoder))


projectActionConnectorDecoder : Decoder ProjectActionConnector
projectActionConnectorDecoder =
    D.succeed ProjectActionConnector
        |> D.required "name" D.string
        |> D.required "element" PluginElement.decoder
        |> D.required "kmPatterns" (D.maybe (D.list IdentifierPattern.decoder))


projectImporterConnectorDecoder : Decoder ProjectImporterConnector
projectImporterConnectorDecoder =
    D.succeed ProjectImporterConnector
        |> D.required "name" D.string
        |> D.required "url" D.string
        |> D.required "element" PluginElement.decoder
        |> D.required "kmPatterns" (D.maybe (D.list IdentifierPattern.decoder))


projectQuestionActionConnectorDecoder : Decoder ProjectQuestionActionConnector
projectQuestionActionConnectorDecoder =
    D.succeed ProjectQuestionActionConnector
        |> D.required "action" actionWithIconDecoder
        |> D.required "type" projectQuestionActionConnectorTypeDecoder
        |> D.required "element" PluginElement.decoder
        |> D.required "kmPatterns" (D.maybe (D.list IdentifierPattern.decoder))


projectQuestionActionConnectorTypeDecoder : Decoder ProjectQuestionActionConnectorType
projectQuestionActionConnectorTypeDecoder =
    D.string
        |> D.andThen
            (\typeStr ->
                case typeStr of
                    "modal" ->
                        D.succeed ModalProjectQuestionAction

                    "sidebar" ->
                        D.succeed SidebarProjectQuestionAction

                    _ ->
                        D.fail ("Unknown ProjectQuestionActionConnectorType: " ++ typeStr)
            )


projectTabConnectorDecoder : Decoder ProjectTabConnector
projectTabConnectorDecoder =
    D.succeed ProjectTabConnector
        |> D.required "tab" actionWithIconDecoder
        |> D.required "url" D.string
        |> D.required "element" PluginElement.decoder
        |> D.required "kmPatterns" (D.maybe (D.list IdentifierPattern.decoder))


settingsConnectorDecoder : Decoder SettingsConnector
settingsConnectorDecoder =
    D.succeed SettingsConnector
        |> D.required "element" PluginElement.decoder
