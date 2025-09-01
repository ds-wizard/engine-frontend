module Wizard.Pages.ProjectFiles.Subscriptions exposing (subscriptions)

import Wizard.Pages.ProjectFiles.Index.Subscriptions
import Wizard.Pages.ProjectFiles.Models exposing (Model)
import Wizard.Pages.ProjectFiles.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.Pages.ProjectFiles.Index.Subscriptions.subscriptions model.indexModel
