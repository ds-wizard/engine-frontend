module Common.Api.Models.Settings.AuthenticationSettings exposing
    ( AuthenticationSettings
    , TwoFactorAuth
    , decoder
    , encode
    , section
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AuthenticationSettings =
    { registrationEnabled : Bool
    , nonAdminLoginEnabled : Bool
    , sessionExpiration : Int
    , userEmailLinkExpiration : Int
    , twoFactorAuth : TwoFactorAuth
    }


type alias TwoFactorAuth =
    { enabled : Bool
    , codeLength : Int
    , expiration : Int
    }


section : { name : String, decoder : Decoder AuthenticationSettings, encode : AuthenticationSettings -> E.Value }
section =
    { name = "authentication", decoder = decoder, encode = encode }


decoder : Decoder AuthenticationSettings
decoder =
    D.succeed AuthenticationSettings
        |> D.required "registrationEnabled" D.bool
        |> D.required "nonAdminLoginEnabled" D.bool
        |> D.required "sessionExpiration" D.int
        |> D.required "userEmailLinkExpiration" D.int
        |> D.required "twoFactorAuth" twoFactorAuthDecoder


twoFactorAuthDecoder : Decoder TwoFactorAuth
twoFactorAuthDecoder =
    D.succeed TwoFactorAuth
        |> D.required "enabled" D.bool
        |> D.required "codeLength" D.int
        |> D.required "expiration" D.int


encode : AuthenticationSettings -> E.Value
encode settings =
    E.object
        [ ( "registrationEnabled", E.bool settings.registrationEnabled )
        , ( "nonAdminLoginEnabled", E.bool settings.nonAdminLoginEnabled )
        , ( "sessionExpiration", E.int settings.sessionExpiration )
        , ( "userEmailLinkExpiration", E.int settings.userEmailLinkExpiration )
        , ( "twoFactorAuth"
          , E.object
                [ ( "enabled", E.bool settings.twoFactorAuth.enabled )
                , ( "codeLength", E.int settings.twoFactorAuth.codeLength )
                , ( "expiration", E.int settings.twoFactorAuth.expiration )
                ]
          )
        ]
