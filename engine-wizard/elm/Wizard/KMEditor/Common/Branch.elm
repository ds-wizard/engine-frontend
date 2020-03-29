module Wizard.KMEditor.Common.Branch exposing
    ( Branch
    , compare
    , decoder
    , listDecoder
    , matchState
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Wizard.KMEditor.Common.BranchState as BranchState exposing (BranchState)


type alias Branch =
    { uuid : String
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
        |> D.required "uuid" D.string
        |> D.required "name" D.string
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


compare : Branch -> Branch -> Order
compare b1 b2 =
    Basics.compare (String.toLower b1.name) (String.toLower b2.name)
