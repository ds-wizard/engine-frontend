module Wizard.Settings.Common.AffiliationConfigForm exposing (AffiliationConfigForm, init, initEmpty, toEditableAffiliationConfig, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)


type alias AffiliationConfigForm =
    { affiliations : Maybe String
    }


initEmpty : Form CustomFormError AffiliationConfigForm
initEmpty =
    Form.initial [] validation


init : EditableAffiliationConfig -> Form CustomFormError AffiliationConfigForm
init config =
    Form.initial (affiliationConfigToFormInitials config) validation


validation : Validation CustomFormError AffiliationConfigForm
validation =
    V.succeed AffiliationConfigForm
        |> V.andMap (V.field "affiliations" V.maybeString)


affiliationConfigToFormInitials : EditableAffiliationConfig -> List ( String, Field.Field )
affiliationConfigToFormInitials config =
    [ ( "affiliations", Field.string <| String.join "\n" config.affiliations )
    ]


toEditableAffiliationConfig : AffiliationConfigForm -> EditableAffiliationConfig
toEditableAffiliationConfig form =
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
    { affiliations = affiliations
    }
