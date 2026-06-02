module Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm exposing
    ( AuthenticationConfigForm
    , init
    , initEmpty
    , toEditableAuthConfig
    , validation
    )

import Common.Data.Role as Role exposing (Role)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig exposing (EditableAuthenticationConfig)


type alias AuthenticationConfigForm =
    { defaultRole : Role
    , registrationEnabled : Bool
    , nonAdminLoginEnabled : Bool
    , twoFactorAuthEnabled : Bool
    , twoFactorAuthCodeLength : Int
    , twoFactorAuthExpiration : Int
    , sessionExpiration : Int
    , userEmailLinkExpiration : Int
    }


initEmpty : Form FormError AuthenticationConfigForm
initEmpty =
    Form.initial [] validation


init : EditableAuthenticationConfig -> Form FormError AuthenticationConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation FormError AuthenticationConfigForm
validation =
    V.succeed AuthenticationConfigForm
        |> V.andMap (V.field "defaultRole" Role.validation)
        |> V.andMap (V.field "registrationEnabled" V.bool)
        |> V.andMap (V.field "nonAdminLoginEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthCodeLength" V.int V.optionalInt)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthExpiration" V.int V.optionalInt)
        |> V.andMap (V.field "sessionExpiration" V.int)
        |> V.andMap (V.field "userEmailLinkExpiration" V.int)


configToFormInitials : EditableAuthenticationConfig -> List ( String, Field )
configToFormInitials config =
    [ ( "defaultRole", Field.string (Role.toString config.defaultRole) )
    , ( "registrationEnabled", Field.bool config.internal.registration.enabled )
    , ( "nonAdminLoginEnabled", Field.bool config.internal.nonAdminLoginEnabled )
    , ( "twoFactorAuthEnabled", Field.bool config.internal.twoFactorAuth.enabled )
    , ( "twoFactorAuthCodeLength", Field.string (String.fromInt config.internal.twoFactorAuth.codeLength) )
    , ( "twoFactorAuthExpiration", Field.string (String.fromInt config.internal.twoFactorAuth.expiration) )
    , ( "sessionExpiration", Field.string (String.fromInt config.internal.sessionExpiration) )
    , ( "userEmailLinkExpiration", Field.string (String.fromInt config.internal.userEmailLinkExpiration) )
    ]


toEditableAuthConfig : AuthenticationConfigForm -> EditableAuthenticationConfig
toEditableAuthConfig form =
    { defaultRole = form.defaultRole
    , internal =
        { registration = { enabled = form.registrationEnabled }
        , nonAdminLoginEnabled = form.nonAdminLoginEnabled
        , twoFactorAuth =
            { enabled = form.twoFactorAuthEnabled
            , codeLength = form.twoFactorAuthCodeLength
            , expiration = form.twoFactorAuthExpiration
            }
        , sessionExpiration = form.sessionExpiration
        , userEmailLinkExpiration = form.userEmailLinkExpiration
        }
    }
