module Registry.Common.AppState exposing
    ( AppState
    , init
    , setCredentials
    )

import Gettext
import Json.Decode as D
import Registry.Common.Credentials exposing (Credentials)
import Registry.Common.Entities.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Registry.Common.Flags as Flags
import Registry.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Registry.Common.Provisioning.DefaultLocale as DefaultLocale
import Shared.Provisioning as Provisioning exposing (Provisioning)


type alias AppState =
    { apiUrl : String
    , valid : Bool
    , config : BootstrapConfig
    , credentials : Maybe Credentials
    , provisioning : Provisioning
    , locale : Gettext.Locale
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
                        ]
            in
            { apiUrl = flags.apiUrl
            , valid = True
            , config = flags.config
            , credentials = flags.credentials
            , provisioning = provisioning
            , locale = Gettext.defaultLocale
            }

        Err _ ->
            { apiUrl = ""
            , valid = False
            , config = BootstrapConfig.default
            , credentials = Nothing
            , provisioning = defaultProvisioning
            , locale = Gettext.defaultLocale
            }


setCredentials : Maybe Credentials -> AppState -> AppState
setCredentials mbCredentials appState =
    { appState | credentials = mbCredentials }
