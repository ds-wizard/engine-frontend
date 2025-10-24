module Wizard.Api.Models.Migration exposing
    ( Migration
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.Migration.MigrationState as MigrationState exposing (MigrationState)


type alias Migration =
    { knowledgeModelEditorUuid : Uuid
    , knowledgeModelEditorName : String
    , migrationState : MigrationState
    , knowledgeModelEditorPreviousKnowledgeModelPackageId : String
    , targetPackageId : String
    , currentKnowledgeModel : KnowledgeModel
    }


decoder : Decoder Migration
decoder =
    D.succeed Migration
        |> D.required "knowledgeModelEditorUuid" Uuid.decoder
        |> D.required "knowledgeModelEditorName" D.string
        |> D.required "migrationState" MigrationState.decoder
        |> D.required "knowledgeModelEditorPreviousKnowledgeModelPackageId" D.string
        |> D.required "targetPackageId" D.string
        |> D.required "currentKnowledgeModel" KnowledgeModel.decoder
