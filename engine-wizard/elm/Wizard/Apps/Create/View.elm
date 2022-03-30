module Wizard.Apps.Create.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div, form)
import Html.Events exposing (onSubmit)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lg)
import Wizard.Apps.Common.AppCreateForm exposing (AppCreateForm)
import Wizard.Apps.Create.Models exposing (Model)
import Wizard.Apps.Create.Msgs exposing (Msg(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Apps.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    form [ onSubmit (FormMsg Form.Submit), detailClass "Apps__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.view appState model.savingApp
        , Html.map FormMsg <| formView appState model.form
        , FormActions.viewSubmit appState
            Routes.appsIndex
            (ActionButton.SubmitConfig (l_ "form.create" appState) model.savingApp)
        ]


formView : AppState -> Form FormError AppCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "appId" <| lg "app.appId" appState
        , FormGroup.input appState form "appName" <| lg "app.name" appState
        , FormGroup.input appState form "email" <| lg "user.email" appState
        , FormGroup.input appState form "firstName" <| lg "user.firstName" appState
        , FormGroup.input appState form "lastName" <| lg "user.lastName" appState
        ]
