module Shared.Data.BranchDetail exposing
    ( BranchDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Event as Event exposing (Event)
import Uuid exposing (Uuid)


type alias BranchDetail =
    { uuid : Uuid
    , name : String
    , kmId : String
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , events : List Event
    }


decoder : Decoder BranchDetail
decoder =
    D.succeed BranchDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list Event.decoder)
