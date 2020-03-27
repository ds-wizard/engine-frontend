module Wizard.Common.Flags exposing (Flags, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Wizard.Common.Config as Config exposing (Config)
import Wizard.Common.Session as Session exposing (Session)


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , clientUrl : String
    , config : Config
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    , success : Bool
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "session" (D.nullable Session.decoder)
        |> D.required "seed" D.int
        |> D.required "apiUrl" D.string
        |> D.required "clientUrl" D.string
        |> D.required "config" Config.decoder
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
        |> D.hardcoded True


default : Flags
default =
    { session = Nothing
    , seed = 0
    , apiUrl = ""
    , clientUrl = ""
    , config = Config.default
    , provisioning = Provisioning.default
    , localProvisioning = Provisioning.default
    , success = False
    }
