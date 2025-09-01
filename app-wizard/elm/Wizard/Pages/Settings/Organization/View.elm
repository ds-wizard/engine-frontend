module Wizard.Pages.Settings.Organization.View exposing (view)

import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Pages.Settings.Organization.Models exposing (Model)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps OrganizationConfigForm Msg
viewProps =
    { locTitle = gettext "Organization"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsOrganization
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError OrganizationConfigForm -> Html Form.Msg
formView appState form =
    let
        affiliations =
            if Admin.isEnabled appState.config.admin then
                []

            else
                [ FormGroup.resizableTextarea appState.locale form "affiliations" (gettext "Affiliations" appState.locale)
                , FormExtra.mdAfter (gettext "Affiliation options will be used to help users choose their affiliation while signing up or editing their profile. Write one affiliation option per line." appState.locale)
                ]
    in
    div []
        ([ FormGroup.input appState.locale form "name" (gettext "Name" appState.locale)
         , FormExtra.textAfter (gettext "Name of the organization running this instance." appState.locale)
         , FormGroup.textarea appState.locale form "description" (gettext "Description" appState.locale)
         , FormGroup.input appState.locale form "organizationId" (gettext "Organization ID" appState.locale)
         , FormExtra.textAfter (gettext "Organization ID is used to identify knowledge models created in this instance. It can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
         ]
            ++ affiliations
        )
