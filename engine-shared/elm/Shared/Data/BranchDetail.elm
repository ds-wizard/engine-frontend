module Shared.Data.BranchDetail exposing
    ( BranchDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Branch.BranchState as BranchState exposing (BranchState)
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package as Package exposing (Package)
import Uuid exposing (Uuid)


type alias BranchDetail =
    { uuid : Uuid
    , name : String
    , kmId : String
    , knowledgeModel : KnowledgeModel
    , forkOfPackageId : Maybe String
    , forkOfPackage : Maybe Package
    , previousPackageId : Maybe String
    , events : List Event
    , state : BranchState
    }


decoder : Decoder BranchDetail
decoder =
    D.succeed BranchDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "forkOfPackage" (D.nullable Package.decoder)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list Event.decoder)
        |> D.required "state" BranchState.decoder
