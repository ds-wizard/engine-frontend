module Wizard.Users.Create.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Events exposing (onSubmit)
import Shared.Data.Role as Role
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Users.Common.UserCreateForm exposing (UserCreateForm)
import Wizard.Users.Create.Models exposing (Model)
import Wizard.Users.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.form [ onSubmit (FormMsg Form.Submit), detailClass "Users__Create" ]
        [ Page.headerWithGuideLink appState (gettext "Create user" appState.locale) GuideLinks.usersCreate
        , FormResult.view model.savingUser
        , formView appState model.form |> Html.map FormMsg
        , FormActions.viewSubmit appState
            Cancel
            (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser)
        ]


formView : AppState -> Form FormError UserCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "email" <| gettext "Email" appState.locale
        , FormGroup.input appState form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <| gettext "Affiliation" appState.locale
        , FormGroup.select appState (Role.options appState) form "role" <| gettext "Role" appState.locale
        , FormGroup.passwordWithStrength appState form "password" <| gettext "Password" appState.locale
        ]
