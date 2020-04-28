module Wizard.Settings.Common.Forms.OrganizationConfigForm exposing
    ( OrganizationConfigForm
    , init
    , initEmpty
    , toOrganizationConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Config.OrganizationConfig exposing (OrganizationConfig)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V


type alias OrganizationConfigForm =
    { name : String
    , organizationId : String
    , description : String
    , affiliations : Maybe String
    }


initEmpty : Form CustomFormError OrganizationConfigForm
initEmpty =
    Form.initial [] validation


init : OrganizationConfig -> Form CustomFormError OrganizationConfigForm
init config =
    Form.initial (organizationConfigToFormInitials config) validation


validation : Validation CustomFormError OrganizationConfigForm
validation =
    V.succeed OrganizationConfigForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "organizationId" V.organizationId)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "affiliations" V.maybeString)


organizationConfigToFormInitials : OrganizationConfig -> List ( String, Field.Field )
organizationConfigToFormInitials config =
    [ ( "name", Field.string config.name )
    , ( "organizationId", Field.string config.organizationId )
    , ( "description", Field.string config.description )
    , ( "affiliations", Field.string <| String.join "\n" config.affiliations )
    ]


toOrganizationConfig : OrganizationConfigForm -> OrganizationConfig
toOrganizationConfig form =
    let
        affiliations =
            case form.affiliations of
                Just formAffiliations ->
                    formAffiliations
                        |> String.split "\n"
                        |> List.map String.trim
                        |> List.filter (not << String.isEmpty)

                Nothing ->
                    []
    in
    { name = form.name
    , organizationId = form.organizationId
    , description = form.description
    , affiliations = affiliations
    }
