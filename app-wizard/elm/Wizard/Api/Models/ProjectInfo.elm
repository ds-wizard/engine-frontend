module Wizard.Api.Models.ProjectInfo exposing
    ( ProjectInfo
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias ProjectInfo =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder ProjectInfo
decoder =
    D.succeed ProjectInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string


encode : ProjectInfo -> E.Value
encode project =
    E.object
        [ ( "uuid", Uuid.encode project.uuid )
        , ( "name", E.string project.name )
        ]
