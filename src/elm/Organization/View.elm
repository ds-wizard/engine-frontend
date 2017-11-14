module Organization.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
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
    case model.organization of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success organization ->
            div []
                [ formResultView model.savingOrganization
                , formView model.form
                , formActionOnly ( "Save", model.savingOrganization, Msgs.OrganizationMsg <| FormMsg Form.Submit )
                ]


formView : Form () OrganizationForm -> Html Msgs.Msg
formView form =
    let
        formHtml =
            div []
                [ inputGroup form "name" "Organization name"
                , inputGroup form "groupId" "Organization Group ID"
                , p [ class "help-block help-block-after" ]
                    [ text "Group ID can contain alfanumeric characters and dot but cannot start or end with dot." ]
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.OrganizationMsg)
