module Wizard.Pages.Settings.Features.View exposing (view)

import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Api.Models.EditableConfig.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Features.Models exposing (Model)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    GenericView.view viewProps appState model


viewProps : GenericView.ViewProps EditableFeaturesConfig Msg
viewProps =
    { locTitle = gettext "Features"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsFeatures
    , wrapMsg = FormMsg
    }


formView : AppState -> Form.Form FormError EditableFeaturesConfig -> Html Form.Msg
formView appState form =
    div [ class "FeaturesForm" ]
        [ FormGroup.toggle form "toursEnabled" (gettext "Tours" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled, Tours help users navigate the application when opening specific screens for the first time." appState.locale)
        ]
