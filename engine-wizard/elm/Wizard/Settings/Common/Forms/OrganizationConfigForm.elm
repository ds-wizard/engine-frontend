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
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Api.Models.BootstrapConfig.OrganizationConfig exposing (OrganizationConfig)
import Wizard.Common.AppState exposing (AppState)


type alias OrganizationConfigForm =
    { name : String
    , organizationId : String
    , description : String
    , affiliations : Maybe String
    }


initEmpty : AppState -> Form FormError OrganizationConfigForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> OrganizationConfig -> Form FormError OrganizationConfigForm
init appState config =
    Form.initial (organizationConfigToFormInitials config) (validation appState)


validation : AppState -> Validation FormError OrganizationConfigForm
validation appState =
    V.succeed OrganizationConfigForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "organizationId" (V.organizationId appState))
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
