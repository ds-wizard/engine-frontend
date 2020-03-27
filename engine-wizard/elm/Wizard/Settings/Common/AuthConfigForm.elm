module Wizard.Settings.Common.AuthConfigForm exposing
    ( AuthConfigForm
    , init
    , initEmpty
    , toEditableAuthConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.AuthServiceConfigForm as AuthServiceConfigForm exposing (AuthServiceConfigForm)
import Wizard.Settings.Common.EditableAuthConfig exposing (EditableAuthConfig)


type alias AuthConfigForm =
    { services : List AuthServiceConfigForm
    , registrationEnabled : Bool
    }


initEmpty : Form CustomFormError AuthConfigForm
initEmpty =
    Form.initial [] validation


init : EditableAuthConfig -> Form CustomFormError AuthConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation CustomFormError AuthConfigForm
validation =
    V.succeed AuthConfigForm
        |> V.andMap (V.field "services" (V.list AuthServiceConfigForm.validation))
        |> V.andMap (V.field "registrationEnabled" V.bool)


configToFormInitials : EditableAuthConfig -> List ( String, Field )
configToFormInitials config =
    let
        services =
            config.external.services
                |> List.map (Field.group << AuthServiceConfigForm.configToFormInitials)
    in
    [ ( "services", Field.list services )
    , ( "registrationEnabled", Field.bool config.internal.registration.enabled )
    ]


toEditableAuthConfig : AuthConfigForm -> EditableAuthConfig
toEditableAuthConfig form =
    { internal = { registration = { enabled = form.registrationEnabled } }
    , external = { services = List.map AuthServiceConfigForm.toEditableAuthServiceConfig form.services }
    }
