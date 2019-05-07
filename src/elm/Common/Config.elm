module Common.Config exposing
    ( Config
    , Widget(..)
    , decoder
    , defaultConfig
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Config =
    { client : ClientConfig
    , feedbackEnabled : Bool
    , registrationEnabled : Bool
    , publicQuestionnaireEnabled : Bool
    , levelsEnabled : Bool
    }


type Widget
    = DMPWorkflow
    | LevelsQuestionnaire
    | Welcome


type alias ClientConfig =
    { appTitle : String
    , appTitleShort : String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , dashboard : Dict String (List Widget)
    }


defaultConfig : Config
defaultConfig =
    { client =
        { appTitle = ""
        , appTitleShort = ""
        , welcomeInfo = Nothing
        , welcomeWarning = Nothing
        , dashboard = Dict.empty
        }
    , feedbackEnabled = True
    , registrationEnabled = True
    , publicQuestionnaireEnabled = True
    , levelsEnabled = True
    }


decoder : Decoder Config
decoder =
    Decode.succeed Config
        |> required "client" clientConfigDecoder
        |> optional "feedbackEnabled" Decode.bool True
        |> optional "registrationEnabled" Decode.bool True
        |> optional "publicQuestionnaireEnabled" Decode.bool True
        |> optional "levelsEnabled" Decode.bool True


clientConfigDecoder : Decoder ClientConfig
clientConfigDecoder =
    Decode.succeed ClientConfig
        |> optional "appTitle" Decode.string "Data Stewardship Wizard"
        |> optional "appTitleShort" Decode.string "DS Wizard"
        |> optional "welcomeInfo" (Decode.maybe Decode.string) Nothing
        |> optional "welcomeWarning" (Decode.maybe Decode.string) Nothing
        |> optional "dashboard" widgetDictDecoder Dict.empty


widgetDictDecoder : Decoder (Dict String (List Widget))
widgetDictDecoder =
    Decode.dict (Decode.list widgetDecoder)


widgetDecoder : Decoder Widget
widgetDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "DMPWorkflow" ->
                        Decode.succeed DMPWorkflow

                    "LevelsQuestionnaire" ->
                        Decode.succeed LevelsQuestionnaire

                    "Welcome" ->
                        Decode.succeed Welcome

                    widgetType ->
                        Decode.fail <| "Unknown widget: " ++ widgetType
            )
