module Users.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith)
import Common.View.FormGroup as FormGroup
import Common.View.Forms exposing (..)
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Msgs
import Routing
import Users.Common.Models exposing (roles)
import Users.Create.Models exposing (..)
import Users.Create.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "Users__Create" ]
        [ Page.header "Create user" []
        , formResultView model.savingUser
        , formView model.form |> Html.map (wrapMsg << FormMsg)
        , formActions (Routing.Users Index) ( "Save", model.savingUser, wrapMsg <| FormMsg Form.Submit )
        ]


formView : Form CustomFormError UserCreateForm -> Html Form.Msg
formView form =
    let
        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) roles

        formHtml =
            div []
                [ FormGroup.input form "email" "Email"
                , FormGroup.input form "name" "Name"
                , FormGroup.input form "surname" "Surname"
                , FormGroup.select roleOptions form "role" "Role"
                , FormGroup.password form "password" "Password"
                ]
    in
    formHtml
