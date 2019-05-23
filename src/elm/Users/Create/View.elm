module Users.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html.Attribute exposing (detailClass)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
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
    div [ detailClass "Users__Create" ]
        [ Page.header "Create user" []
        , FormResult.view model.savingUser
        , formView model.form |> Html.map (wrapMsg << FormMsg)
        , FormActions.view
            (Routing.Users Index)
            (ActionButton.ButtonConfig "Save" model.savingUser (wrapMsg <| FormMsg Form.Submit) False)
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
