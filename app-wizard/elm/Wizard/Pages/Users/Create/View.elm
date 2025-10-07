module Wizard.Pages.Users.Create.View exposing (view)

import Common.Components.ActionButton as ActionButton
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Data.Role as Role
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Events exposing (onSubmit)
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserCreateForm exposing (UserCreateForm)
import Wizard.Pages.Users.Create.Models exposing (Model)
import Wizard.Pages.Users.Create.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Html.form [ onSubmit (FormMsg Form.Submit), detailClass "Users__Create" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.usersCreate) (gettext "Create user" appState.locale)
        , FormResult.view model.savingUser
        , formView appState model.form |> Html.map FormMsg
        , FormActions.viewSubmit appState
            Cancel
            (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser)
        ]


formView : AppState -> Form FormError UserCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState.locale form "email" <| gettext "Email" appState.locale
        , FormGroup.input appState.locale form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState.locale form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState.locale form "affiliation" <| gettext "Affiliation" appState.locale
        , FormGroup.select appState.locale (Role.options appState) form "role" <| gettext "Role" appState.locale
        , FormGroup.passwordWithStrength appState.locale form "password" <| gettext "Password" appState.locale
        ]
