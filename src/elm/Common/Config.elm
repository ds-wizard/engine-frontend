module Common.Config exposing
    ( Config
    , CustomMenuLink
    , Registry(..)
    , Widget(..)
    , decoder
    , defaultConfig
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Config =
    { client : ClientConfig
    , feedbackEnabled : Bool
    , registrationEnabled : Bool
    , publicQuestionnaireEnabled : Bool
    , questionnaireAccessibilityEnabled : Bool
    , levelsEnabled : Bool
    , registry : Registry
    }


type Widget
    = DMPWorkflow
    | LevelsQuestionnaire
    | Welcome


type Registry
    = RegistryEnabled String
    | RegistryDisabled


type alias ClientConfig =
    { appTitle : String
    , appTitleShort : String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , dashboard : Dict String (List Widget)
    , privacyUrl : String
    , customMenuLinks : List CustomMenuLink
    , supportEmail : String
    , supportRepositoryName : String
    , supportRepositoryUrl : String
    }


type alias CustomMenuLink =
    { icon : String
    , title : String
    , url : String
    , newWindow : Bool
    }


defaultPrivacyUrl : String
defaultPrivacyUrl =
    "https://ds-wizard.org/privacy.html"


defaultSupportEmail : String
defaultSupportEmail =
    "support@ds-wizard.org"


defaultSupportRepositoryName : String
defaultSupportRepositoryName =
    "ds-wizard/ds-wizard"


defaultSupportRepositoryUrl : String
defaultSupportRepositoryUrl =
    "https://github.com/ds-wizard/ds-wizard/issues"


defaultConfig : Config
defaultConfig =
    { client =
        { appTitle = ""
        , appTitleShort = ""
        , welcomeInfo = Nothing
        , welcomeWarning = Nothing
        , dashboard = Dict.empty
        , privacyUrl = defaultPrivacyUrl
        , customMenuLinks = []
        , supportEmail = defaultSupportEmail
        , supportRepositoryName = defaultSupportRepositoryName
        , supportRepositoryUrl = defaultSupportRepositoryUrl
        }
    , feedbackEnabled = True
    , registrationEnabled = True
    , publicQuestionnaireEnabled = True
    , questionnaireAccessibilityEnabled = True
    , levelsEnabled = True
    , registry = RegistryDisabled
    }


decoder : Decoder Config
decoder =
    D.succeed Config
        |> D.required "client" clientConfigDecoder
        |> D.optional "feedbackEnabled" D.bool True
        |> D.optional "registrationEnabled" D.bool True
        |> D.optional "publicQuestionnaireEnabled" D.bool True
        |> D.optional "questionnaireAccessibilityEnabled" D.bool True
        |> D.optional "levelsEnabled" D.bool True
        |> D.optional "registry" registryDecoder RegistryDisabled


registryDecoder : Decoder Registry
registryDecoder =
    D.succeed Tuple.pair
        |> D.required "enabled" D.bool
        |> D.required "url" (D.maybe D.string)
        |> D.andThen
            (\( enabled, mbUrl ) ->
                case ( enabled, mbUrl ) of
                    ( True, Just url ) ->
                        D.succeed <| RegistryEnabled url

                    _ ->
                        D.succeed RegistryDisabled
            )


clientConfigDecoder : Decoder ClientConfig
clientConfigDecoder =
    D.succeed ClientConfig
        |> D.optional "appTitle" D.string "Data Stewardship Wizard"
        |> D.optional "appTitleShort" D.string "DS Wizard"
        |> D.optional "welcomeInfo" (D.maybe D.string) Nothing
        |> D.optional "welcomeWarning" (D.maybe D.string) Nothing
        |> D.optional "dashboard" widgetDictDecoder Dict.empty
        |> D.optional "privacyUrl" D.string defaultPrivacyUrl
        |> D.optional "customMenuLinks" (D.list customMenuLinkDecoder) []
        |> D.optional "supportEmail" D.string defaultSupportEmail
        |> D.optional "supportRepositoryName" D.string defaultSupportRepositoryName
        |> D.optional "supportRepositoryUrl" D.string defaultSupportRepositoryUrl


customMenuLinkDecoder : Decoder CustomMenuLink
customMenuLinkDecoder =
    D.succeed CustomMenuLink
        |> D.required "icon" D.string
        |> D.required "title" D.string
        |> D.required "url" D.string
        |> D.required "newWindow" D.bool


widgetDictDecoder : Decoder (Dict String (List Widget))
widgetDictDecoder =
    D.dict (D.list widgetDecoder)


widgetDecoder : Decoder Widget
widgetDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "DMPWorkflow" ->
                        D.succeed DMPWorkflow

                    "LevelsQuestionnaire" ->
                        D.succeed LevelsQuestionnaire

                    "Welcome" ->
                        D.succeed Welcome

                    widgetType ->
                        D.fail <| "Unknown widget: " ++ widgetType
            )
