module KMEditor.Common.Migration exposing
    ( Migration
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.MigrationState as MigrationState exposing (MigrationState)


type alias Migration =
    { branchUuid : String
    , migrationState : MigrationState
    , branchPreviousPackageId : String
    , targetPackageId : String
    , currentKnowledgeModel : KnowledgeModel
    }


decoder : Decoder Migration
decoder =
    D.succeed Migration
        |> D.required "branchUuid" D.string
        |> D.required "migrationState" MigrationState.decoder
        |> D.required "branchPreviousPackageId" D.string
        |> D.required "targetPackageId" D.string
        |> D.required "currentKnowledgeModel" KnowledgeModel.decoder
