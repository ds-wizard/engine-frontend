module Wizard.Settings.Organization.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
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
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError OrganizationConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "name" (gettext "Name" appState.locale)
        , FormExtra.textAfter (gettext "Name of the organization running this instance." appState.locale)
        , FormGroup.textarea appState form "description" (gettext "Description" appState.locale)
        , FormGroup.input appState form "organizationId" (gettext "Organization ID" appState.locale)
        , FormExtra.textAfter (gettext "Organization ID is used to identify Knowledge Models created in this instance. It can contain alphanumeric characters and dots but cannot start or end with a dot." appState.locale)
        , FormGroup.resizableTextarea appState form "affiliations" (gettext "Affiliations" appState.locale)
        , FormExtra.mdAfter (gettext "Affiliation options will be used to help users choose their affiliation while signing up or editing their profile. Write one affiliation option per line." appState.locale)
        ]
