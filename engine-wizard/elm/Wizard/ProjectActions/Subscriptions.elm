module Wizard.ProjectActions.Subscriptions exposing (subscriptions)

import Wizard.ProjectActions.Index.Subscriptions
import Wizard.ProjectActions.Models exposing (Model)
import Wizard.ProjectActions.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.ProjectActions.Index.Subscriptions.subscriptions model.indexModel
