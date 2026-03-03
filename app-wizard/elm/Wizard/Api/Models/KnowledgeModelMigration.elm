module Wizard.Api.Models.KnowledgeModelMigration exposing
    ( KnowledgeModelMigration
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelMigration.KnowledgeModelMigrationState as KnowledgeModelMigrationState exposing (KnowledgeModelMigrationState)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)


type alias KnowledgeModelMigration =
    { editorUuid : Uuid
    , editorName : String
    , editorPreviousPackage : KnowledgeModelPackageSuggestion
    , state : KnowledgeModelMigrationState
    , targetPackage : KnowledgeModelPackageSuggestion
    , currentKnowledgeModel : KnowledgeModel
    }


decoder : Decoder KnowledgeModelMigration
decoder =
    D.succeed KnowledgeModelMigration
        |> D.required "editorUuid" Uuid.decoder
        |> D.required "editorName" D.string
        |> D.required "editorPreviousPackage" KnowledgeModelPackageSuggestion.decoder
        |> D.required "state" KnowledgeModelMigrationState.decoder
        |> D.required "targetPackage" KnowledgeModelPackageSuggestion.decoder
        |> D.required "currentKnowledgeModel" KnowledgeModel.decoder
