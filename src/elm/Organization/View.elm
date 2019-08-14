module Organization.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Msgs
import Organization.Common.Organization exposing (Organization)
import Organization.Common.OrganizationForm exposing (OrganizationForm)
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Organization.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewOrganization appState model) model.organization


viewOrganization : AppState -> Model -> Organization -> Html Msg
viewOrganization appState model _ =
    div [ detailClass "Organization" ]
        [ Page.header (lg "organization" appState) []
        , div []
            [ FormResult.view model.savingOrganization
            , formView appState model.form
            , FormActions.viewActionOnly (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingOrganization (FormMsg Form.Submit) False)
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
