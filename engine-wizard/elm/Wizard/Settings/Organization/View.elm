module Wizard.Settings.Organization.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (compose2)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Organization.Models exposing (Model)


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps OrganizationConfigForm Msg
viewProps =
    { locTitle = gettext "Organization"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = GuideLinks.settingsOrganization
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError OrganizationConfigForm -> Html Form.Msg
formView appState form =
    let
        affiliations =
            if Admin.isEnabled appState.config.admin then
                []

            else
                [ FormGroup.resizableTextarea appState form "affiliations" (gettext "Affiliations" appState.locale)
                , FormExtra.mdAfter (gettext "Affiliation options will be used to help users choose their affiliation while signing up or editing their profile. Write one affiliation option per line." appState.locale)
                ]
    in
    div []
        ([ FormGroup.input appState form "name" (gettext "Name" appState.locale)
         , FormExtra.textAfter (gettext "Name of the organization running this instance." appState.locale)
         , FormGroup.textarea appState form "description" (gettext "Description" appState.locale)
         , FormGroup.input appState form "organizationId" (gettext "Organization ID" appState.locale)
         , FormExtra.textAfter (gettext "Organization ID is used to identify knowledge models created in this instance. It can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
         ]
            ++ affiliations
        )
