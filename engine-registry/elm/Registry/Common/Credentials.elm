module Registry.Common.Credentials exposing
    ( Credentials
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Credentials =
    { organizationId : String
    , token : String
    }


encode : Credentials -> E.Value
encode credentials =
    E.object
        [ ( "organizationId", E.string credentials.organizationId )
        , ( "token", E.string credentials.token )
        ]


decoder : Decoder Credentials
decoder =
    D.succeed Credentials
        |> D.required "organizationId" D.string
        |> D.required "token" D.string
