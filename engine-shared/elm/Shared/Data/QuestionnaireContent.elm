module Shared.Data.QuestionnaireContent exposing (..)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply as Reply exposing (Reply)
import Shared.Data.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)


type alias QuestionnaireContent =
    { replies : Dict String Reply
    , level : Int
    , labels : Dict String (List String)
    , events : List QuestionnaireEvent
    , versions : List QuestionnaireVersion
    }


decoder : Decoder QuestionnaireContent
decoder =
    D.succeed QuestionnaireContent
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "level" D.int
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "events" (D.list QuestionnaireEvent.decoder)
        |> D.required "versions" (D.list QuestionnaireVersion.decoder)
