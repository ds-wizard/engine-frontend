module Common.Api.Models.ProjectSettings exposing
    ( ProjectSettings
    , decoder
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Common.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Common.Api.Models.KnowledgeModel.Tag as Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelLocale as KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.Project.DocumentTemplateProjectState as DocumentTemplateProjectState exposing (DocumentTemplateProjectState)
import Common.Api.Models.Project.KnowledgeModelProjectState as KnowledgeModelProjectState exposing (KnowledgeModelProjectState)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias ProjectSettings =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , knowledgeModelPackage : KnowledgeModelPackage
    , knowledgeModelState : KnowledgeModelProjectState
    , projectTags : List String
    , selectedQuestionTagUuids : List String
    , documentTemplate : Maybe DocumentTemplateSuggestion
    , documentTemplatePhase : Maybe DocumentTemplatePhase
    , documentTemplateSupportState : Maybe DocumentTemplateState
    , documentTemplateState : Maybe DocumentTemplateProjectState
    , formatUuid : Maybe Uuid
    , documentTemplateLanguage : Maybe String
    , isTemplate : Bool
    , knowledgeModelTags : List Tag
    , language : Maybe String
    , availableLocales : List KnowledgeModelLocale
    }


decoder : Decoder ProjectSettings
decoder =
    D.succeed ProjectSettings
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "knowledgeModelPackage" KnowledgeModelPackage.decoder
        |> D.required "knowledgeModelState" KnowledgeModelProjectState.decoder
        |> D.required "projectTags" (D.list D.string)
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "documentTemplate" (D.nullable DocumentTemplateSuggestion.decoder)
        |> D.required "documentTemplatePhase" (D.nullable DocumentTemplatePhase.decoder)
        |> D.required "documentTemplateSupportState" (D.nullable DocumentTemplateState.decoder)
        |> D.required "documentTemplateState" (D.nullable DocumentTemplateProjectState.decoder)
        |> D.required "formatUuid" (D.nullable Uuid.decoder)
        |> D.required "documentTemplateLanguage" (D.nullable D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelTags" (D.list Tag.decoder)
        |> D.required "language" (D.nullable D.string)
        |> D.required "availableLocales" (D.list KnowledgeModelLocale.decoder)
