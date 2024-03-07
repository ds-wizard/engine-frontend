module Registry2.Data.Flags exposing
    ( Flags
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry2.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Registry2.Data.Session as Session exposing (Session)


type alias Flags =
    { apiUrl : String
    , appTitle : Maybe String
    , config : BootstrapConfig
    , session : Maybe Session
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "config" BootstrapConfig.decoder
        |> D.required "session" (D.maybe Session.decoder)
