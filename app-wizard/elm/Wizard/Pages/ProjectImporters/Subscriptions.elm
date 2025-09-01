module Wizard.Pages.ProjectImporters.Subscriptions exposing (subscriptions)

import Wizard.Pages.ProjectImporters.Index.Subscriptions
import Wizard.Pages.ProjectImporters.Models exposing (Model)
import Wizard.Pages.ProjectImporters.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.Pages.ProjectImporters.Index.Subscriptions.subscriptions model.indexModel
