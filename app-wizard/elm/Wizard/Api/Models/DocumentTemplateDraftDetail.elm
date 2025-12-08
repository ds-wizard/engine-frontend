module Wizard.Api.Models.DocumentTemplateDraftDetail exposing
    ( DocumentTemplateDraftDetail
    , decoder
    , getPreviewSettings
    , isPreviewSet
    , updatePreviewSettings
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedKnowledgeModelPackage exposing (AllowedKnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Wizard.Api.Models.ProjectSuggestion as ProjectSuggestion exposing (ProjectSuggestion)


type alias DocumentTemplateDraftDetail =
    { allowedPackages : List AllowedKnowledgeModelPackage
    , createdAt : Time.Posix
    , description : String
    , formats : List DocumentTemplateFormatDraft
    , id : String
    , license : String
    , name : String
    , readme : String
    , templateId : String
    , version : Version
    , formatUuid : Maybe Uuid
    , projectUuid : Maybe Uuid
    , project : Maybe ProjectSuggestion
    , knowledgeModelEditorUuid : Maybe Uuid
    , knowledgeModelEditor : Maybe KnowledgeModelEditorSuggestion
    }


decoder : Decoder DocumentTemplateDraftDetail
decoder =
    D.succeed DocumentTemplateDraftDetail
        |> D.required "allowedPackages" (D.list DocumentTemplateAllowedPackage.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list DocumentTemplateFormatDraft.decoder)
        |> D.required "id" D.string
        |> D.required "license" D.string
        |> D.required "name" D.string
        |> D.required "readme" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.optional "formatUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "projectUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "project" (D.maybe ProjectSuggestion.decoder) Nothing
        |> D.optional "knowledgeModelEditorUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "knowledgeModelEditor" (D.maybe KnowledgeModelEditorSuggestion.decoder) Nothing


getPreviewSettings : DocumentTemplateDraftDetail -> DocumentTemplateDraftPreviewSettings
getPreviewSettings detail =
    { formatUuid = detail.formatUuid
    , projectUuid = detail.projectUuid
    , project = detail.project
    , knowledgeModelEditorUuid = detail.knowledgeModelEditorUuid
    , knowledgeModelEditor = detail.knowledgeModelEditor
    }


updatePreviewSettings : DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftDetail -> DocumentTemplateDraftDetail
updatePreviewSettings previewSettings detail =
    { detail
        | formatUuid = previewSettings.formatUuid
        , projectUuid = previewSettings.projectUuid
        , project = previewSettings.project
        , knowledgeModelEditorUuid = previewSettings.knowledgeModelEditorUuid
        , knowledgeModelEditor = previewSettings.knowledgeModelEditor
    }


isPreviewSet : DocumentTemplateDraftDetail -> Bool
isPreviewSet =
    DocumentTemplateDraftPreviewSettings.isPreviewSet << getPreviewSettings
