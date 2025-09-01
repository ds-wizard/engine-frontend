module Wizard.Api.Models.EditableConfig.EditableTwoFactorAuthConfig exposing
    ( EditableTwoFactorAuthConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias EditableTwoFactorAuthConfig =
    { enabled : Bool
    , codeLength : Int
    , expiration : Int
    }



-- JSON


decoder : Decoder EditableTwoFactorAuthConfig
decoder =
    D.succeed EditableTwoFactorAuthConfig
        |> D.required "enabled" D.bool
        |> D.required "codeLength" D.int
        |> D.required "expiration" D.int


encode : EditableTwoFactorAuthConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "codeLength", E.int config.codeLength )
        , ( "expiration", E.int config.expiration )
        ]
