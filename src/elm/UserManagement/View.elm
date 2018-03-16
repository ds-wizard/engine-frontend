module UserManagement.View exposing (view)

import Html exposing (Html)
import Msgs
import UserManagement.Create.View
import UserManagement.Edit.View
import UserManagement.Index.View
import UserManagement.Models exposing (Model)
import UserManagement.Msgs exposing (Msg(..))
import UserManagement.Routing exposing (Route(..))


view : Route -> (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view route wrapMsg model =
    case route of
        Create ->
            UserManagement.Create.View.view (wrapMsg << CreateMsg) model.createModel

        Edit uuid ->
            UserManagement.Edit.View.view (wrapMsg << EditMsg) model.editModel

        Index ->
            UserManagement.Index.View.view (wrapMsg << IndexMsg) model.indexModel
