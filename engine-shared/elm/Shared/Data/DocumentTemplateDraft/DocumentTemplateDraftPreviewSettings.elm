module Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings, decoder, encode, init, isPreviewSet)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Shared.Data.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Uuid exposing (Uuid)


type alias DocumentTemplateDraftPreviewSettings =
    { formatUuid : Maybe Uuid
    , questionnaireUuid : Maybe Uuid
    , questionnaire : Maybe QuestionnaireSuggestion
    }


init : DocumentTemplateDraftPreviewSettings
init =
    { formatUuid = Nothing
    , questionnaireUuid = Nothing
    , questionnaire = Nothing
    }


decoder : Decoder DocumentTemplateDraftPreviewSettings
decoder =
    D.succeed DocumentTemplateDraftPreviewSettings
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaireUuid" (D.maybe Uuid.decoder)
        |> D.required "questionnaire" (D.maybe QuestionnaireSuggestion.decoder)


encode : DocumentTemplateDraftPreviewSettings -> E.Value
encode settings =
    E.object
        [ ( "formatUuid", E.maybe Uuid.encode settings.formatUuid )
        , ( "questionnaireUuid", E.maybe Uuid.encode settings.questionnaireUuid )
        ]


isPreviewSet : DocumentTemplateDraftPreviewSettings -> Bool
isPreviewSet settings =
    Maybe.isJust settings.formatUuid && Maybe.isJust settings.questionnaireUuid
