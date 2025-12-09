module Wizard.Pages.Users.Create.View exposing (view)

import Common.Components.Container as Container
import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Data.Role as Role
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserCreateForm exposing (UserCreateForm)
import Wizard.Pages.Users.Create.Models exposing (Model)
import Wizard.Pages.Users.Create.Msgs exposing (Msg(..))
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Container.simpleForm
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.usersCreate) (gettext "Create User" appState.locale)
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingUser
            , formView = formView appState model.form |> Html.map FormMsg
            , submitLabel = gettext "Create" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
        ]


formView : AppState -> Form FormError UserCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState.locale form "email" <| gettext "Email" appState.locale
        , FormGroup.input appState.locale form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState.locale form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState.locale form "affiliation" <| gettext "Affiliation" appState.locale
        , FormGroup.select appState.locale (Role.options appState) form "role" <| gettext "Role" appState.locale
        , FormGroup.secret appState.locale form "password" <| gettext "Password" appState.locale
        ]
