module Wizard.Api.Models.BranchDetail exposing
    ( BranchDetail
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.Branch.BranchState as BranchState exposing (BranchState)
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.Package as Package exposing (Package)
import Wizard.Api.Models.QuestionnaireDetail.Reply as Reply exposing (Reply)


type alias BranchDetail =
    { uuid : Uuid
    , name : String
    , description : String
    , kmId : String
    , license : String
    , readme : String
    , version : Version
    , knowledgeModel : KnowledgeModel
    , forkOfPackageId : Maybe String
    , forkOfPackage : Maybe Package
    , previousPackageId : Maybe String
    , events : List Event
    , state : BranchState
    , replies : Dict String Reply
    }


decoder : Decoder BranchDetail
decoder =
    D.succeed BranchDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "kmId" D.string
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "version" Version.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "forkOfPackage" (D.nullable Package.decoder)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list Event.decoder)
        |> D.required "state" BranchState.decoder
        |> D.required "replies" (D.dict Reply.decoder)
