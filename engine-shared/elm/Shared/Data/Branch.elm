module Shared.Data.Branch exposing
    ( Branch
    , decoder
    , matchState
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.Branch.BranchState as BranchState exposing (BranchState)
import Time
import Uuid exposing (Uuid)


type alias Branch =
    { uuid : Uuid
    , name : String
    , kmId : String
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , state : BranchState
    , updatedAt : Time.Posix
    }


decoder : Decoder Branch
decoder =
    D.succeed Branch
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "state" BranchState.decoder
        |> D.required "updatedAt" D.datetime


matchState : List BranchState -> Branch -> Bool
matchState states branch =
    List.any ((==) branch.state) states
