module Shared.Data.DocumentTemplateDraftDetail exposing
    ( DocumentTemplateDraftDetail
    , decoder
    , getPreviewSettings
    , isPreviewSet
    , updatePreviewSettings
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.BranchSuggestion as BranchSuggestion exposing (BranchSuggestion)
import Shared.Data.DocumentTemplate.DocumentTemplateAllowedPackage as AllowedPackage
import Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Shared.Data.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage exposing (AllowedPackage)
import Shared.Data.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateDraftDetail =
    { allowedPackages : List AllowedPackage
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
    , questionnaireUuid : Maybe Uuid
    , questionnaire : Maybe QuestionnaireSuggestion
    , branchUuid : Maybe Uuid
    , branch : Maybe BranchSuggestion
    }


decoder : Decoder DocumentTemplateDraftDetail
decoder =
    D.succeed DocumentTemplateDraftDetail
        |> D.required "allowedPackages" (D.list AllowedPackage.decoder)
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
        |> D.optional "questionnaireUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "questionnaire" (D.maybe QuestionnaireSuggestion.decoder) Nothing
        |> D.optional "branchUuid" (D.maybe Uuid.decoder) Nothing
        |> D.optional "branch" (D.maybe BranchSuggestion.decoder) Nothing


getPreviewSettings : DocumentTemplateDraftDetail -> DocumentTemplateDraftPreviewSettings
getPreviewSettings detail =
    { formatUuid = detail.formatUuid
    , questionnaireUuid = detail.questionnaireUuid
    , questionnaire = detail.questionnaire
    , branchUuid = detail.branchUuid
    , branch = detail.branch
    }


updatePreviewSettings : DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftDetail -> DocumentTemplateDraftDetail
updatePreviewSettings previewSettings detail =
    { detail
        | formatUuid = previewSettings.formatUuid
        , questionnaireUuid = previewSettings.questionnaireUuid
        , questionnaire = previewSettings.questionnaire
        , branchUuid = previewSettings.branchUuid
        , branch = previewSettings.branch
    }


isPreviewSet : DocumentTemplateDraftDetail -> Bool
isPreviewSet =
    DocumentTemplateDraftPreviewSettings.isPreviewSet << getPreviewSettings
