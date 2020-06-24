module WizardResearch.Common.Flags exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias Flags =
    { seed : Int
    , apiUrl : String
    , config : BootstrapConfig
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    , session : Maybe Session
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "seed" D.int
        |> D.required "apiUrl" D.string
        |> D.required "config" BootstrapConfig.decoder
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
        |> D.optional "session" (D.maybe Session.decoder) Nothing


default : Flags
default =
    { seed = 0
    , apiUrl = ""
    , config = BootstrapConfig.default
    , provisioning = Provisioning.default
    , localProvisioning = Provisioning.default
    , session = Nothing
    }
