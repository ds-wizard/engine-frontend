module Wizard.Users.Create.View exposing (view)

import Form exposing (Form)
import Html exposing (..)
import Shared.Auth.Role as Role
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Users.Common.UserCreateForm exposing (UserCreateForm)
import Wizard.Users.Create.Models exposing (..)
import Wizard.Users.Create.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Users.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ detailClass "Users__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.view appState model.savingUser
        , formView appState model.form |> Html.map FormMsg
        , FormActions.view appState
            (Routes.UsersRoute IndexRoute)
            (ActionButton.ButtonConfig (l_ "header.save" appState) model.savingUser (FormMsg Form.Submit) False)
        ]


formView : AppState -> Form FormError UserCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "email" <| lg "user.email" appState
        , FormGroup.input appState form "firstName" <| lg "user.firstName" appState
        , FormGroup.input appState form "lastName" <| lg "user.lastName" appState
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <| lg "user.affiliation" appState
        , FormGroup.select appState (Role.options appState) form "role" <| lg "user.role" appState
        , FormGroup.password appState form "password" <| lg "user.password" appState
        ]
