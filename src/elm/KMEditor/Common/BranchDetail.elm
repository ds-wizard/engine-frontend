module KMEditor.Common.BranchDetail exposing
    ( BranchDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import KMEditor.Common.Models.Events exposing (Event, eventDecoder)


type alias BranchDetail =
    { uuid : String
    , name : String
    , kmId : String
    , organizationId : String
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , events : List Event
    }


decoder : Decoder BranchDetail
decoder =
    D.succeed BranchDetail
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "organizationId" D.string
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list eventDecoder)
