module Wizard.Api.Models.ProjectSettings exposing
    ( ProjectSettings
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Wizard.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.KnowledgeModel.Tag as Tag exposing (Tag)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)


type alias ProjectSettings =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , knowledgeModelPackage : KnowledgeModelPackage
    , projectTags : List String
    , selectedQuestionTagUuids : List String
    , documentTemplate : Maybe DocumentTemplateSuggestion
    , documentTemplatePhase : Maybe DocumentTemplatePhase
    , documentTemplateState : Maybe DocumentTemplateState
    , formatUuid : Maybe Uuid
    , isTemplate : Bool
    , knowledgeModelTags : List Tag
    }


decoder : Decoder ProjectSettings
decoder =
    D.succeed ProjectSettings
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "knowledgeModelPackage" KnowledgeModelPackage.decoder
        |> D.required "projectTags" (D.list D.string)
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "documentTemplate" (D.maybe DocumentTemplateSuggestion.decoder)
        |> D.required "documentTemplatePhase" (D.maybe DocumentTemplatePhase.decoder)
        |> D.required "documentTemplateState" (D.maybe DocumentTemplateState.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelTags" (D.list Tag.decoder)
