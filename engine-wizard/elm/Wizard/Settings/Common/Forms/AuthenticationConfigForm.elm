module Wizard.Settings.Common.Forms.AuthenticationConfigForm exposing
    ( AuthenticationConfigForm
    , init
    , initEmpty
    , toEditableAuthConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Wizard.Settings.Common.Forms.OpenIDServiceConfigForm as OpenIDServiceConfigForm exposing (OpenIDServiceConfigForm)


type alias AuthenticationConfigForm =
    { defaultRole : String
    , services : List OpenIDServiceConfigForm
    , registrationEnabled : Bool
    }


initEmpty : Form CustomFormError AuthenticationConfigForm
initEmpty =
    Form.initial [] validation


init : EditableAuthenticationConfig -> Form CustomFormError AuthenticationConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation CustomFormError AuthenticationConfigForm
validation =
    V.succeed AuthenticationConfigForm
        |> V.andMap (V.field "defaultRole" V.string)
        |> V.andMap (V.field "services" (V.list OpenIDServiceConfigForm.validation))
        |> V.andMap (V.field "registrationEnabled" V.bool)


configToFormInitials : EditableAuthenticationConfig -> List ( String, Field )
configToFormInitials config =
    let
        services =
            config.external.services
                |> List.map (Field.group << OpenIDServiceConfigForm.configToFormInitials)
    in
    [ ( "defaultRole", Field.string config.defaultRole )
    , ( "services", Field.list services )
    , ( "registrationEnabled", Field.bool config.internal.registration.enabled )
    ]


toEditableAuthConfig : AuthenticationConfigForm -> EditableAuthenticationConfig
toEditableAuthConfig form =
    { defaultRole = form.defaultRole
    , internal = { registration = { enabled = form.registrationEnabled } }
    , external = { services = List.map OpenIDServiceConfigForm.toEditableOpenIDServiceConfig form.services }
    }
