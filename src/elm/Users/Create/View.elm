module Users.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClass)
import Common.View exposing (pageHeader)
import Common.View.Forms exposing (..)
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
    div [ detailContainerClass ]
        [ pageHeader "Create user" []
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
                [ inputGroup form "email" "Email"
                , inputGroup form "name" "Name"
                , inputGroup form "surname" "Surname"
                , selectGroup roleOptions form "role" "Role"
                , passwordGroup form "password" "Password"
                ]
    in
    formHtml
