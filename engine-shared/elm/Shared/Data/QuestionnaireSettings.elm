module Shared.Data.QuestionnaireSettings exposing
    ( QuestionnaireSettings
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Shared.Data.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.KnowledgeModel.Tag as Tag exposing (Tag)
import Shared.Data.Package as Package exposing (Package)
import Uuid exposing (Uuid)


type alias QuestionnaireSettings =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , package : Package
    , projectTags : List String
    , selectedQuestionTagUuids : List String
    , documentTemplate : Maybe DocumentTemplateSuggestion
    , documentTemplatePhase : Maybe DocumentTemplatePhase
    , documentTemplateState : Maybe DocumentTemplateState
    , formatUuid : Maybe Uuid
    , isTemplate : Bool
    , knowledgeModelTags : List Tag
    }


decoder : Decoder QuestionnaireSettings
decoder =
    D.succeed QuestionnaireSettings
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "package" Package.decoder
        |> D.required "projectTags" (D.list D.string)
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "documentTemplate" (D.maybe DocumentTemplateSuggestion.decoder)
        |> D.required "documentTemplatePhase" (D.maybe DocumentTemplatePhase.decoder)
        |> D.required "documentTemplateState" (D.maybe DocumentTemplateState.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelTags" (D.list Tag.decoder)
