module Registry.Api.Models.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

{-| What the registry says about itself before anything else: whether visitors
may make an account, and whether it offers locales.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias BootstrapConfig =
    { publicRegistrationEnabled : Bool
    , localesEnabled : Bool
    }


default : BootstrapConfig
default =
    { publicRegistrationEnabled = False
    , localesEnabled = False
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.requiredAt [ "authentication", "publicRegistrationEnabled" ] D.bool
        |> D.requiredAt [ "locale", "enabled" ] D.bool
