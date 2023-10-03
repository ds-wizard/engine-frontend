module Registry.Common.AppState exposing
    ( AppState
    , init
    , setCredentials
    )

import Gettext exposing (gettext)
import Json.Decode as D
import Registry.Common.Credentials exposing (Credentials)
import Registry.Common.Entities.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Registry.Common.Flags as Flags
import Registry.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Registry.Common.Provisioning.DefaultLocale as DefaultLocale
import Shared.Provisioning exposing (Provisioning)


type alias AppState =
    { apiUrl : String
    , appTitle : String
    , config : BootstrapConfig
    , credentials : Maybe Credentials
    , locale : Gettext.Locale
    , provisioning : Provisioning
    , valid : Bool
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

        locale =
            Gettext.defaultLocale

        defaultAppTitle =
            gettext "DSW Registry" locale
    in
    case flagsResult of
        Ok flags ->
            { apiUrl = flags.apiUrl
            , appTitle = Maybe.withDefault defaultAppTitle flags.appTitle
            , valid = True
            , config = flags.config
            , credentials = flags.credentials
            , provisioning = defaultProvisioning
            , locale = locale
            }

        Err _ ->
            { apiUrl = ""
            , appTitle = defaultAppTitle
            , valid = False
            , config = BootstrapConfig.default
            , credentials = Nothing
            , provisioning = defaultProvisioning
            , locale = locale
            }


setCredentials : Maybe Credentials -> AppState -> AppState
setCredentials mbCredentials appState =
    { appState | credentials = mbCredentials }
