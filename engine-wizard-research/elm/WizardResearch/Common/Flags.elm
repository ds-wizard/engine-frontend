module WizardResearch.Common.Flags exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias Flags =
    { seed : Int
    , apiUrl : String
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "seed" D.int
        |> D.required "apiUrl" D.string
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default


default : Flags
default =
    { seed = 0
    , apiUrl = ""
    , provisioning = Provisioning.default
    , localProvisioning = Provisioning.default
    }
