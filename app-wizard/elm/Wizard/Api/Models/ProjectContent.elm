module Wizard.Api.Models.ProjectContent exposing
    ( ProjectContent
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)
import Wizard.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)


type alias ProjectContent =
    { replies : Dict String Reply
    , phaseUuid : Maybe Uuid
    , labels : Dict String (List String)
    , events : List ProjectEvent
    , versions : List ProjectVersion
    }


decoder : Decoder ProjectContent
decoder =
    D.succeed ProjectContent
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "events" (D.list ProjectEvent.decoder)
        |> D.required "versions" (D.list ProjectVersion.decoder)
