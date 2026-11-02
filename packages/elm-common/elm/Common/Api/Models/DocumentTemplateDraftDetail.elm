module Common.Api.Models.DocumentTemplateDraftDetail exposing
    ( DocumentTemplateDraftDetail
    , decoder
    , getPreviewSettings
    , isPreviewSet
    , updatePreviewSettings
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage exposing (DocumentTemplateAllowedPackage)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Common.Api.Models.ProjectSuggestion as ProjectSuggestion exposing (ProjectSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateDraftDetail =
    { allowedPackages : List DocumentTemplateAllowedPackage
    , createdAt : Time.Posix
    , description : String
    , formats : List DocumentTemplateFormatDraft
    , uuid : Uuid
    , language : String
    , license : String
    , name : String
    , readme : String
    , id : String
    , version : Version
    , formatUuid : Maybe Uuid
    , projectUuid : Maybe Uuid
    , project : Maybe ProjectSuggestion
    , knowledgeModelEditorUuid : Maybe Uuid
    , knowledgeModelEditor : Maybe KnowledgeModelEditorSuggestion
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder DocumentTemplateDraftDetail
decoder =
    D.succeed DocumentTemplateDraftDetail
        |> D.required "allowedPackages" (D.list DocumentTemplateAllowedPackage.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list DocumentTemplateFormatDraft.decoder)
        |> D.required "uuid" Uuid.decoder
        |> D.required "language" D.string
        |> D.required "license" D.string
        |> D.required "name" D.string
        |> D.required "readme" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder
        |> D.optional "formatUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "projectUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "project" (D.maybe ProjectSuggestion.decoder) Nothing
        |> D.optional "knowledgeModelEditorUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "knowledgeModelEditor" (D.maybe KnowledgeModelEditorSuggestion.decoder) Nothing
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


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
