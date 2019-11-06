module Registry.Common.Flags exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Credentials as Credentials exposing (Credentials)
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias Flags =
    { apiUrl : String
    , credentials : Maybe Credentials
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "credentials" (D.maybe Credentials.decoder)
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
