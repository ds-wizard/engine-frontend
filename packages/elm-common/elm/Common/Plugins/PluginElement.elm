module Common.Plugins.PluginElement exposing
    ( PluginElement
    , decoder
    , documentValue
    , element
    , encode
    , integrationReplyValue
    , knowledgeModelEditorValue
    , knowledgeModelValue
    , onActionClose
    , onImport
    , onPluginIntegrationSettingsChange
    , onReplyValueChange
    , onSettingsValueChange
    , onUserSettingsValueChange
    , pluginIntegrationSettingsValue
    , projectString
    , projectValue
    , projectValueString
    , questionPathValue
    , questionValue
    , settingValue
    , userSettingsValue
    , userValue
    )

import Common.Api.Models.Document as Document exposing (Document)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Common.Data.ImporterEvent as ImporterEvent exposing (ImporterEvent)
import Html exposing (Attribute, Html, node)
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


type PluginElement
    = PluginElement String


decoder : Decoder PluginElement
decoder =
    D.map PluginElement D.string


encode : PluginElement -> E.Value
encode (PluginElement pluginElement) =
    E.string pluginElement


element : PluginElement -> List (Attribute msg) -> Html msg
element (PluginElement pluginElement) attributes =
    node pluginElement attributes []



-- Attributes


documentValue : Document -> Html.Attribute msg
documentValue document =
    Html.Attributes.attribute "document-value" <|
        E.encode 0 (Document.encode document)


integrationReplyValue : String -> Html.Attribute msg
integrationReplyValue =
    Html.Attributes.attribute "integration-reply-value"


{-| The editor an integration is edited in: the knowledge model is the
editor's, and so is the workspace, which the knowledge model content does not
know (a package of the organization's library belongs to none).
-}
knowledgeModelEditorValue : { a | uuid : Uuid, name : String, workspaceUuid : Maybe Uuid } -> Html.Attribute msg
knowledgeModelEditorValue editor =
    Html.Attributes.attribute "knowledge-model-editor-value" <|
        E.encode 0
            (E.object
                [ ( "uuid", Uuid.encode editor.uuid )
                , ( "name", E.string editor.name )
                , ( "workspaceUuid", E.maybe Uuid.encode editor.workspaceUuid )
                ]
            )


knowledgeModelValue : String -> Html.Attribute msg
knowledgeModelValue kmString =
    Html.Attributes.attribute "knowledge-model-value" kmString


pluginIntegrationSettingsValue : String -> Html.Attribute msg
pluginIntegrationSettingsValue =
    Html.Attributes.attribute "plugin-integration-settings-value"


projectValue : ProjectCommon -> Html.Attribute msg
projectValue =
    projectValueString << projectString


{-| The project as the attribute carries it, for a view that keeps it as a
string to stay lazy.
-}
projectString : ProjectCommon -> String
projectString project =
    E.encode 0 (ProjectCommon.encode project)


projectValueString : String -> Html.Attribute msg
projectValueString =
    Html.Attributes.attribute "project-value"


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


userValue : String -> Html.Attribute msg
userValue =
    Html.Attributes.attribute "user-value"



-- Events


onActionClose : msg -> Html.Attribute msg
onActionClose msg =
    Html.Events.on "action-close" (D.succeed msg)


onImport : (List ImporterEvent -> msg) -> Html.Attribute msg
onImport =
    onEvent "import" [ "detail", "events" ] (D.list ImporterEvent.decoder)


onPluginIntegrationSettingsChange : (String -> msg) -> Html.Attribute msg
onPluginIntegrationSettingsChange =
    onEvent "plugin-integration-settings-change" [ "detail", "value" ] D.string


onReplyValueChange : (String -> msg) -> Html.Attribute msg
onReplyValueChange =
    onEvent "reply-value-change" [ "detail", "value" ] D.string


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
