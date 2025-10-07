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
    { branchUuid : Uuid
    , branchName : String
    , migrationState : MigrationState
    , branchPreviousPackageId : String
    , targetPackageId : String
    , currentKnowledgeModel : KnowledgeModel
    }


decoder : Decoder Migration
decoder =
    D.succeed Migration
        |> D.required "branchUuid" Uuid.decoder
        |> D.required "branchName" D.string
        |> D.required "migrationState" MigrationState.decoder
        |> D.required "branchPreviousPackageId" D.string
        |> D.required "targetPackageId" D.string
        |> D.required "currentKnowledgeModel" KnowledgeModel.decoder
