module Wizard.ProjectFiles.Subscriptions exposing (subscriptions)

import Wizard.ProjectFiles.Index.Subscriptions
import Wizard.ProjectFiles.Models exposing (Model)
import Wizard.ProjectFiles.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.ProjectFiles.Index.Subscriptions.subscriptions model.indexModel
