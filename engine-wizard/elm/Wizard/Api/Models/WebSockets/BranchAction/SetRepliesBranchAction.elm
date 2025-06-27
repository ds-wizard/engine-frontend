module Wizard.Api.Models.WebSockets.BranchAction.SetRepliesBranchAction exposing
    ( SetRepliesBranchAction
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireDetail.Reply as Reply exposing (Reply)


type alias SetRepliesBranchAction =
    { uuid : Uuid
    , replies : Dict String Reply
    }


decoder : Decoder SetRepliesBranchAction
decoder =
    D.succeed SetRepliesBranchAction
        |> D.required "uuid" Uuid.decoder
        |> D.required "replies" (D.dict Reply.decoder)


encode : SetRepliesBranchAction -> E.Value
encode action =
    E.object
        [ ( "uuid", Uuid.encode action.uuid )
        , ( "replies", E.dict identity Reply.encode action.replies )
        ]
