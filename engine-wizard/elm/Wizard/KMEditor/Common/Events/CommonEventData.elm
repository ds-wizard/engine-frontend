module Wizard.KMEditor.Common.Events.CommonEventData exposing
    ( CommonEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias CommonEventData =
    { uuid : String
    , parentUuid : String
    , entityUuid : String
    }


decoder : Decoder CommonEventData
decoder =
    D.succeed CommonEventData
        |> D.required "uuid" D.string
        |> D.required "parentUuid" D.string
        |> D.required "entityUuid" D.string


encode : CommonEventData -> List ( String, E.Value )
encode data =
    [ ( "uuid", E.string data.uuid )
    , ( "parentUuid", E.string data.parentUuid )
    , ( "entityUuid", E.string data.entityUuid )
    ]
