module Wizard.Organization.View exposing (view)

import Form exposing (Form)
import Html exposing (..)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Organization.Common.Organization exposing (Organization)
import Wizard.Organization.Common.OrganizationForm exposing (OrganizationForm)
import Wizard.Organization.Models exposing (..)
import Wizard.Organization.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Organization.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewOrganization appState model) model.organization


viewOrganization : AppState -> Model -> Organization -> Html Msg
viewOrganization appState model _ =
    div [ detailClass "Organization" ]
        [ Page.header (lg "organization" appState) []
        , div []
            [ FormResult.view appState model.savingOrganization
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingOrganization (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError OrganizationForm -> Html Msg
formView appState form =
    let
        formHtml =
            div []
                [ FormGroup.input appState form "name" <| lg "organization.name" appState
                , FormGroup.input appState form "organizationId" <| lg "organization.id" appState
                , FormExtra.textAfter <| l_ "form.organizationId.description" appState
                ]
    in
    formHtml |> Html.map FormMsg
