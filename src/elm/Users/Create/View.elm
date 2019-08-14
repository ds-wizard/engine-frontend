module Users.Create.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Routes
import Users.Common.User as User
import Users.Common.UserCreateForm exposing (UserCreateForm)
import Users.Create.Models exposing (..)
import Users.Create.Msgs exposing (Msg(..))
import Users.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Users.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ detailClass "Users__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.view model.savingUser
        , formView appState model.form |> Html.map FormMsg
        , FormActions.view appState
            (Routes.UsersRoute IndexRoute)
            (ActionButton.ButtonConfig (l_ "header.save" appState) model.savingUser (FormMsg Form.Submit) False)
        ]


formView : AppState -> Form CustomFormError UserCreateForm -> Html Form.Msg
formView appState form =
    let
        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) User.roles

        formHtml =
            div []
                [ FormGroup.input appState form "email" <| lg "user.email" appState
                , FormGroup.input appState form "name" <| lg "user.name" appState
                , FormGroup.input appState form "surname" <| lg "user.surname" appState
                , FormGroup.select appState roleOptions form "role" <| lg "user.role" appState
                , FormGroup.password appState form "password" <| lg "user.password" appState
                ]
    in
    formHtml
