module Wizard.Plugins.PluginElement exposing
    ( PluginElement
    , decoder
    , documentValue
    , element
    , knowledgeModelValue
    , onActionClose
    , onImport
    , onSettingsValueChange
    , onUserSettingsValueChange
    , projectValue
    , questionPathValue
    , questionValue
    , settingValue
    , userSettingsValue
    )

import Html exposing (Attribute, Html, node)
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Document as Document exposing (Document)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Wizard.Components.Questionnaire.Importer.ImporterEvent as ImporterEvent exposing (ImporterEvent)


type PluginElement
    = PluginElement String


decoder : Decoder PluginElement
decoder =
    D.map PluginElement D.string


element : PluginElement -> List (Attribute msg) -> Html msg
element (PluginElement pluginElement) attributes =
    node pluginElement attributes []



-- Attributes


documentValue : Document -> Html.Attribute msg
documentValue document =
    Html.Attributes.attribute "document-value" <|
        E.encode 0 (Document.encode document)


knowledgeModelValue : String -> Html.Attribute msg
knowledgeModelValue kmString =
    Html.Attributes.attribute "knowledge-model-value" kmString


projectValue : ProjectCommon -> Html.Attribute msg
projectValue project =
    Html.Attributes.attribute "project-value" <|
        E.encode 0 (ProjectCommon.encode project)


questionValue : Question -> Html.Attribute msg
questionValue question =
    Html.Attributes.attribute "question-value" <|
        E.encode 0 (Question.encode question)


questionPathValue : String -> Html.Attribute msg
questionPathValue path =
    Html.Attributes.attribute "question-path-value" <|
        E.encode 0 (E.string path)


settingValue : String -> Html.Attribute msg
settingValue =
    Html.Attributes.attribute "settings-value"


userSettingsValue : String -> Html.Attribute msg
userSettingsValue =
    Html.Attributes.attribute "user-settings-value"



-- Events


onActionClose : msg -> Html.Attribute msg
onActionClose msg =
    Html.Events.on "action-close" (D.succeed msg)


onImport : (List ImporterEvent -> msg) -> Html.Attribute msg
onImport =
    onEvent "import" [ "detail", "events" ] (D.list ImporterEvent.decoder)


onSettingsValueChange : (String -> msg) -> Html.Attribute msg
onSettingsValueChange =
    onEvent "settings-value-change" [ "detail", "value" ] D.string


onUserSettingsValueChange : (String -> msg) -> Html.Attribute msg
onUserSettingsValueChange =
    onEvent "user-settings-value-change" [ "detail", "value" ] D.string



-- Helpers


onEvent : String -> List String -> Decoder a -> (a -> msg) -> Html.Attribute msg
onEvent eventName dataPath dataDecoder toMsg =
    Html.Events.on eventName <|
        D.map toMsg <|
            D.at dataPath dataDecoder
