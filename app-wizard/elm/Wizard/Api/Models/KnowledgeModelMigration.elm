module Wizard.Api.Models.KnowledgeModelMigration exposing
    ( KnowledgeModelMigration
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelMigration.KnowledgeModelMigrationState as KnowledgeModelMigrationState exposing (KnowledgeModelMigrationState)


type alias KnowledgeModelMigration =
    { editorUuid : Uuid
    , editorName : String
    , editorPreviousPackageId : String
    , state : KnowledgeModelMigrationState
    , targetPackageId : String
    , currentKnowledgeModel : KnowledgeModel
    }


decoder : Decoder KnowledgeModelMigration
decoder =
    D.succeed KnowledgeModelMigration
        |> D.required "editorUuid" Uuid.decoder
        |> D.required "editorName" D.string
        |> D.required "editorPreviousPackageId" D.string
        |> D.required "state" KnowledgeModelMigrationState.decoder
        |> D.required "targetPackageId" D.string
        |> D.required "currentKnowledgeModel" KnowledgeModel.decoder
