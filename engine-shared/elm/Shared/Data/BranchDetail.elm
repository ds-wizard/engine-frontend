module Shared.Data.BranchDetail exposing
    ( BranchDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Branch.BranchState as BranchState exposing (BranchState)
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion as PackageSuggestion exposing (PackageSuggestion)
import Uuid exposing (Uuid)


type alias BranchDetail =
    { uuid : Uuid
    , name : String
    , kmId : String
    , knowledgeModel : KnowledgeModel
    , forkOfPackageId : Maybe String
    , forkOfPackage : Maybe PackageSuggestion
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
        |> D.required "forkOfPackage" (D.nullable PackageSuggestion.decoder)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list Event.decoder)
        |> D.required "state" BranchState.decoder
