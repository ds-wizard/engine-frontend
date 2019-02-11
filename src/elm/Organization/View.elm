module Organization.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith)
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Msgs
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))


view : Model -> Html Msgs.Msg
view model =
    div [ detailContainerClassWith "Organization" ]
        [ Page.header "Organization" []
        , Page.actionResultView (viewOrganization model) model.organization
        ]


viewOrganization : Model -> Organization -> Html Msgs.Msg
viewOrganization model _ =
    div []
        [ FormResult.view model.savingOrganization
        , formView model.form
        , FormActions.viewActionOnly ( "Save", model.savingOrganization, Msgs.OrganizationMsg <| FormMsg Form.Submit )
        ]


formView : Form CustomFormError OrganizationForm -> Html Msgs.Msg
formView form =
    let
        formHtml =
            div []
                [ FormGroup.input form "name" "Organization name"
                , FormGroup.input form "organizationId" "Organization ID"
                , FormExtra.textAfter "Organization ID can contain alphanumeric characters and dot but cannot start or end with dot."
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.OrganizationMsg)
