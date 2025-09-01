module Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings, clearQuestionnaireAndBranch, decoder, encode, init, isPreviewSet)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.BranchSuggestion as BranchSuggestion exposing (BranchSuggestion)
import Wizard.Api.Models.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)


type alias DocumentTemplateDraftPreviewSettings =
    { formatUuid : Maybe Uuid
    , questionnaireUuid : Maybe Uuid
    , questionnaire : Maybe QuestionnaireSuggestion
    , branchUuid : Maybe Uuid
    , branch : Maybe BranchSuggestion
    }


init : DocumentTemplateDraftPreviewSettings
init =
    { formatUuid = Nothing
    , questionnaireUuid = Nothing
    , questionnaire = Nothing
    , branchUuid = Nothing
    , branch = Nothing
    }


clearQuestionnaireAndBranch : DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftPreviewSettings
clearQuestionnaireAndBranch settings =
    { settings | questionnaireUuid = Nothing, questionnaire = Nothing, branchUuid = Nothing, branch = Nothing }


decoder : Decoder DocumentTemplateDraftPreviewSettings
decoder =
    D.succeed DocumentTemplateDraftPreviewSettings
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaireUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaire" (D.maybe QuestionnaireSuggestion.decoder)
        |> D.required "branchUuid" (D.maybe Uuid.decoder)
        |> D.required "branch" (D.maybe BranchSuggestion.decoder)


encode : DocumentTemplateDraftPreviewSettings -> E.Value
encode settings =
    E.object
        [ ( "formatUuid", E.maybe Uuid.encode settings.formatUuid )
        , ( "questionnaireUuid", E.maybe Uuid.encode settings.questionnaireUuid )
        , ( "branchUuid", E.maybe Uuid.encode settings.branchUuid )
        ]


isPreviewSet : DocumentTemplateDraftPreviewSettings -> Bool
isPreviewSet settings =
    Maybe.isJust settings.formatUuid
        && (Maybe.isJust settings.questionnaireUuid || Maybe.isJust settings.branchUuid)
