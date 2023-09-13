module Registry.Common.Flags exposing
    ( Flags
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Credentials as Credentials exposing (Credentials)
import Registry.Common.Entities.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias Flags =
    { apiUrl : String
    , config : BootstrapConfig
    , credentials : Maybe Credentials
    , localProvisioning : Provisioning
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "config" BootstrapConfig.decoder
        |> D.required "credentials" (D.maybe Credentials.decoder)
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
