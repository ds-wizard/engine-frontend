module Registry.Common.AppState exposing
    ( AppState
    , init
    , setCredentials
    )

import Json.Decode as D exposing (Decoder)
import Registry.Common.Credentials exposing (Credentials)
import Registry.Common.Flags as Flags
import Registry.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Registry.Common.Provisioning.DefaultLocale as DefaultLocale
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias AppState =
    { apiUrl : String
    , valid : Bool
    , credentials : Maybe Credentials
    , provisioning : Provisioning
    }


init : D.Value -> AppState
init flagsValue =
    let
        flagsResult =
            D.decodeValue Flags.decoder flagsValue

        defaultProvisioning =
            { locale = DefaultLocale.locale
            , iconSet = DefaultIconSet.iconSet
            }
    in
    case flagsResult of
        Ok flags ->
            let
                provisioning =
                    Provisioning.foldl
                        [ defaultProvisioning
                        , flags.localProvisioning
                        , flags.provisioning
                        ]
            in
            AppState flags.apiUrl True flags.credentials provisioning

        Err _ ->
            AppState "" False Nothing defaultProvisioning


setCredentials : Maybe Credentials -> AppState -> AppState
setCredentials mbCredentials appState =
    { appState | credentials = mbCredentials }
