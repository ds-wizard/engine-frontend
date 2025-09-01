module Wizard.Api.Models.QuestionnaireContent exposing
    ( QuestionnaireContent
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetail.Reply as Reply exposing (Reply)
import Wizard.Api.Models.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)


type alias QuestionnaireContent =
    { replies : Dict String Reply
    , phaseUuid : Maybe Uuid
    , labels : Dict String (List String)
    , events : List QuestionnaireEvent
    , versions : List QuestionnaireVersion
    }


decoder : Decoder QuestionnaireContent
decoder =
    D.succeed QuestionnaireContent
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "events" (D.list QuestionnaireEvent.decoder)
        |> D.required "versions" (D.list QuestionnaireVersion.decoder)
