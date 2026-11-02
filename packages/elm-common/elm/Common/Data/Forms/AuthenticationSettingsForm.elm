module Common.Data.Forms.AuthenticationSettingsForm exposing
    ( AuthenticationSettingsForm
    , init
    , initEmpty
    , toSettings
    , validation
    )

import Common.Api.Models.Settings.AuthenticationSettings exposing (AuthenticationSettings)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)


type alias AuthenticationSettingsForm =
    { registrationEnabled : Bool
    , nonAdminLoginEnabled : Bool
    , twoFactorAuthEnabled : Bool
    , twoFactorAuthCodeLength : Int
    , twoFactorAuthExpiration : Int
    , sessionExpiration : Int
    , userEmailLinkExpiration : Int
    }


initEmpty : Form FormError AuthenticationSettingsForm
initEmpty =
    Form.initial [] validation


init : AuthenticationSettings -> Form FormError AuthenticationSettingsForm
init settings =
    Form.initial (toFormInitials settings) validation


validation : Validation FormError AuthenticationSettingsForm
validation =
    V.succeed AuthenticationSettingsForm
        |> V.andMap (V.field "registrationEnabled" V.bool)
        |> V.andMap (V.field "nonAdminLoginEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthCodeLength" V.int V.optionalInt)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthExpiration" V.int V.optionalInt)
        |> V.andMap (V.field "sessionExpiration" V.int)
        |> V.andMap (V.field "userEmailLinkExpiration" V.int)


toFormInitials : AuthenticationSettings -> List ( String, Field )
toFormInitials settings =
    [ ( "registrationEnabled", Field.bool settings.registrationEnabled )
    , ( "nonAdminLoginEnabled", Field.bool settings.nonAdminLoginEnabled )
    , ( "twoFactorAuthEnabled", Field.bool settings.twoFactorAuth.enabled )
    , ( "twoFactorAuthCodeLength", Field.string (String.fromInt settings.twoFactorAuth.codeLength) )
    , ( "twoFactorAuthExpiration", Field.string (String.fromInt settings.twoFactorAuth.expiration) )
    , ( "sessionExpiration", Field.string (String.fromInt settings.sessionExpiration) )
    , ( "userEmailLinkExpiration", Field.string (String.fromInt settings.userEmailLinkExpiration) )
    ]


toSettings : AuthenticationSettingsForm -> AuthenticationSettings
toSettings form =
    { registrationEnabled = form.registrationEnabled
    , nonAdminLoginEnabled = form.nonAdminLoginEnabled
    , sessionExpiration = form.sessionExpiration
    , userEmailLinkExpiration = form.userEmailLinkExpiration
    , twoFactorAuth =
        { enabled = form.twoFactorAuthEnabled
        , codeLength = form.twoFactorAuthCodeLength
        , expiration = form.twoFactorAuthExpiration
        }
    }
