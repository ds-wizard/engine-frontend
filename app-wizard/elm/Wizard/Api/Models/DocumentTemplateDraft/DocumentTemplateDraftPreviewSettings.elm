module Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings, clearQuestionnaireAndKmEditor, decoder, encode, init, isPreviewSet)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Wizard.Api.Models.ProjectSuggestion as ProjectSuggestion exposing (ProjectSuggestion)


type alias DocumentTemplateDraftPreviewSettings =
    { formatUuid : Maybe Uuid
    , projectUuid : Maybe Uuid
    , project : Maybe ProjectSuggestion
    , knowledgeModelEditorUuid : Maybe Uuid
    , knowledgeModelEditor : Maybe KnowledgeModelEditorSuggestion
    }


init : DocumentTemplateDraftPreviewSettings
init =
    { formatUuid = Nothing
    , projectUuid = Nothing
    , project = Nothing
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    }


clearQuestionnaireAndKmEditor : DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftPreviewSettings
clearQuestionnaireAndKmEditor settings =
    { settings | projectUuid = Nothing, project = Nothing, knowledgeModelEditorUuid = Nothing, knowledgeModelEditor = Nothing }


decoder : Decoder DocumentTemplateDraftPreviewSettings
decoder =
    D.succeed DocumentTemplateDraftPreviewSettings
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "projectUuid" (D.maybe Uuid.decoder)
        |> D.required "project" (D.maybe ProjectSuggestion.decoder)
        |> D.required "knowledgeModelEditorUuid" (D.maybe Uuid.decoder)
        |> D.required "knowledgeModelEditor" (D.maybe KnowledgeModelEditorSuggestion.decoder)


encode : DocumentTemplateDraftPreviewSettings -> E.Value
encode settings =
    E.object
        [ ( "formatUuid", E.maybe Uuid.encode settings.formatUuid )
        , ( "projectUuid", E.maybe Uuid.encode settings.projectUuid )
        , ( "knowledgeModelEditorUuid", E.maybe Uuid.encode settings.knowledgeModelEditorUuid )
        ]


isPreviewSet : DocumentTemplateDraftPreviewSettings -> Bool
isPreviewSet settings =
    Maybe.isJust settings.formatUuid
        && (Maybe.isJust settings.projectUuid || Maybe.isJust settings.knowledgeModelEditorUuid)
