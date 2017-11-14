module UserManagement.Create.View exposing (..)

import Common.View exposing (pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Routing exposing (Route(..))
import UserManagement.Create.Models exposing (Model)
import UserManagement.Create.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Create user" []
        , formResultView model.savingUser
        , formView model.form
        , formActions UserManagement ( "Save", model.savingUser, Msgs.UserManagementCreateMsg <| FormMsg Form.Submit )
        ]


formView : Form () UserCreateForm -> Html Msgs.Msg
formView form =
    let
        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) roles

        formHtml =
            div []
                [ inputGroup form "email" "Email"
                , inputGroup form "name" "Name"
                , inputGroup form "surname" "Surname"
                , selectGroup roleOptions form "role" "Role"
                , passwordGroup form "password" "Password"
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.UserManagementCreateMsg)
