module Wizard.Common.Flags exposing (Flags, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Auth.Session as Session exposing (Session)
import Shared.Common.Navigator as Navigator exposing (Navigator)
import Shared.Data.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , clientUrl : String
    , config : BootstrapConfig
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    , navigator : Navigator
    , success : Bool
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "session" (D.nullable Session.decoder)
        |> D.required "seed" D.int
        |> D.required "apiUrl" D.string
        |> D.required "clientUrl" D.string
        |> D.required "config" BootstrapConfig.decoder
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
        |> D.required "navigator" Navigator.decoder
        |> D.hardcoded True


default : Flags
default =
    { session = Nothing
    , seed = 0
    , apiUrl = ""
    , clientUrl = ""
    , config = BootstrapConfig.default
    , provisioning = Provisioning.default
    , localProvisioning = Provisioning.default
    , navigator = Navigator.default
    , success = False
    }
