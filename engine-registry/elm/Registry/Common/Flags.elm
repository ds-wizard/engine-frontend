module Registry.Common.Flags exposing
    ( Flags
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Credentials as Credentials exposing (Credentials)
import Registry.Common.Entities.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)


type alias Flags =
    { apiUrl : String
    , appTitle : Maybe String
    , config : BootstrapConfig
    , credentials : Maybe Credentials
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "config" BootstrapConfig.decoder
        |> D.required "credentials" (D.maybe Credentials.decoder)
