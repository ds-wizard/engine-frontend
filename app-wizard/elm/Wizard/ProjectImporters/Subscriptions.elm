module Wizard.ProjectImporters.Subscriptions exposing (subscriptions)

import Wizard.ProjectImporters.Index.Subscriptions
import Wizard.ProjectImporters.Models exposing (Model)
import Wizard.ProjectImporters.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.ProjectImporters.Index.Subscriptions.subscriptions model.indexModel
