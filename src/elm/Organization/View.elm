module Organization.View exposing (..)

import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Organization" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    if model.loading then
        fullPageLoader
    else if model.loadingError /= "" then
        defaultFullPageError model.loadingError
    else
        div []
            [ formResultView model.result
            , formView model.form
            , formActionOnly ( "Save", model.saving, Msgs.OrganizationMsg <| FormMsg Form.Submit )
            ]


formView : Form () OrganizationForm -> Html Msgs.Msg
formView form =
    let
        formHtml =
            div []
                [ inputGroup form "name" "Organization name"
                , inputGroup form "namespace" "Organization namespace"
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.OrganizationMsg)
