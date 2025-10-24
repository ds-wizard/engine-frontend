module Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings, clearQuestionnaireAndKmEditor, decoder, encode, init, isPreviewSet)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Wizard.Api.Models.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)


type alias DocumentTemplateDraftPreviewSettings =
    { formatUuid : Maybe Uuid
    , questionnaireUuid : Maybe Uuid
    , questionnaire : Maybe QuestionnaireSuggestion
    , knowledgeModelEditorUuid : Maybe Uuid
    , knowledgeModelEditor : Maybe KnowledgeModelEditorSuggestion
    }


init : DocumentTemplateDraftPreviewSettings
init =
    { formatUuid = Nothing
    , questionnaireUuid = Nothing
    , questionnaire = Nothing
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    }


clearQuestionnaireAndKmEditor : DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftPreviewSettings
clearQuestionnaireAndKmEditor settings =
    { settings | questionnaireUuid = Nothing, questionnaire = Nothing, knowledgeModelEditorUuid = Nothing, knowledgeModelEditor = Nothing }


decoder : Decoder DocumentTemplateDraftPreviewSettings
decoder =
    D.succeed DocumentTemplateDraftPreviewSettings
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaireUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaire" (D.maybe QuestionnaireSuggestion.decoder)
        |> D.required "knowledgeModelEditorUuid" (D.maybe Uuid.decoder)
        |> D.required "knowledgeModelEditor" (D.maybe KnowledgeModelEditorSuggestion.decoder)


encode : DocumentTemplateDraftPreviewSettings -> E.Value
encode settings =
    E.object
        [ ( "formatUuid", E.maybe Uuid.encode settings.formatUuid )
        , ( "questionnaireUuid", E.maybe Uuid.encode settings.questionnaireUuid )
        , ( "knowledgeModelEditorUuid", E.maybe Uuid.encode settings.knowledgeModelEditorUuid )
        ]


isPreviewSet : DocumentTemplateDraftPreviewSettings -> Bool
isPreviewSet settings =
    Maybe.isJust settings.formatUuid
        && (Maybe.isJust settings.questionnaireUuid || Maybe.isJust settings.knowledgeModelEditorUuid)
