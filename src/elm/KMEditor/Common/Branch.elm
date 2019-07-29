module KMEditor.Common.Branch exposing
    ( Branch
    , decoder
    , listDecoder
    , matchState
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import KMEditor.Common.BranchState as BranchState exposing (BranchState)
import Time


type alias Branch =
    { uuid : String
    , name : String
    , organizationId : String
    , kmId : String
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , state : BranchState
    , updatedAt : Time.Posix
    }


decoder : Decoder Branch
decoder =
    D.succeed Branch
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "state" BranchState.decoder
        |> D.required "updatedAt" D.datetime


listDecoder : Decoder (List Branch)
listDecoder =
    D.list decoder


matchState : List BranchState -> Branch -> Bool
matchState states branch =
    List.any ((==) branch.state) states
