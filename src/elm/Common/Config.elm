module Common.Config exposing
    ( Config
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
    , itemTitleEnabled : Bool
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
    }


defaultPrivacyUrl : String
defaultPrivacyUrl =
    "https://ds-wizard.org/privacy.html"


defaultConfig : Config
defaultConfig =
    { client =
        { appTitle = ""
        , appTitleShort = ""
        , welcomeInfo = Nothing
        , welcomeWarning = Nothing
        , dashboard = Dict.empty
        , privacyUrl = defaultPrivacyUrl
        }
    , feedbackEnabled = True
    , registrationEnabled = True
    , publicQuestionnaireEnabled = True
    , questionnaireAccessibilityEnabled = True
    , levelsEnabled = True
    , itemTitleEnabled = True
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
        |> D.optional "itemTitleEnabled" D.bool True
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
