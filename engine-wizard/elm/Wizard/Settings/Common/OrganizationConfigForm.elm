module Wizard.Settings.Common.OrganizationConfigForm exposing
    ( OrganizationConfigForm
    , init
    , initEmpty
    , toEditableOrganizationConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableOrganizationConfig exposing (EditableOrganizationConfig)


type alias OrganizationConfigForm =
    { uuid : String
    , name : String
    , organizationId : String
    }


initEmpty : Form CustomFormError OrganizationConfigForm
initEmpty =
    Form.initial [] validation


init : EditableOrganizationConfig -> Form CustomFormError OrganizationConfigForm
init config =
    Form.initial (organizationConfigToFormInitials config) validation


validation : Validation CustomFormError OrganizationConfigForm
validation =
    V.succeed OrganizationConfigForm
        |> V.andMap (V.field "uuid" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "organizationId" (V.regex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))


organizationConfigToFormInitials : EditableOrganizationConfig -> List ( String, Field.Field )
organizationConfigToFormInitials config =
    [ ( "uuid", Field.string config.uuid )
    , ( "name", Field.string config.name )
    , ( "organizationId", Field.string config.organizationId )
    ]


toEditableOrganizationConfig : OrganizationConfigForm -> EditableOrganizationConfig
toEditableOrganizationConfig form =
    { uuid = form.uuid
    , name = form.name
    , organizationId = form.organizationId
    }
