module Organization.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode)
import Common.View.Forms exposing (..)
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
        [ formResultView model.savingOrganization
        , formView model.form
        , formActionOnly ( "Save", model.savingOrganization, Msgs.OrganizationMsg <| FormMsg Form.Submit )
        ]


formView : Form CustomFormError OrganizationForm -> Html Msgs.Msg
formView form =
    let
        formHtml =
            div []
                [ inputGroup form "name" "Organization name"
                , inputGroup form "organizationId" "Organization ID"
                , formTextAfter "Organization ID can contain alfanumeric characters and dot but cannot start or end with dot."
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.OrganizationMsg)
