module Users.View exposing (view)

import Html exposing (Html)
import Msgs
import Users.Create.View
import Users.Edit.View
import Users.Index.View
import Users.Models exposing (Model)
import Users.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))


view : Route -> (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view route wrapMsg model =
    case route of
        Create ->
            Users.Create.View.view (wrapMsg << CreateMsg) model.createModel

        Edit uuid ->
            Users.Edit.View.view (wrapMsg << EditMsg) model.editModel

        Index ->
            Users.Index.View.view (wrapMsg << IndexMsg) model.indexModel
