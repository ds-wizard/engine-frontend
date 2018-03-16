module UserManagement.Create.View exposing (view)

import Common.Html exposing (detailContainerClass)
import Common.View exposing (pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Msgs
import Routing
import UserManagement.Common.Models exposing (roles)
import UserManagement.Create.Models exposing (..)
import UserManagement.Create.Msgs exposing (Msg(..))
import UserManagement.Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClass ]
        [ pageHeader "Create user" []
        , formResultView model.savingUser
        , formView model.form |> Html.map (wrapMsg << FormMsg)
        , formActions (Routing.UserManagement Index) ( "Save", model.savingUser, wrapMsg <| FormMsg Form.Submit )
        ]


formView : Form () UserCreateForm -> Html Form.Msg
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
    formHtml
