module Wizard.Pages.ProjectActions.Subscriptions exposing (subscriptions)

import Wizard.Pages.ProjectActions.Index.Subscriptions
import Wizard.Pages.ProjectActions.Models exposing (Model)
import Wizard.Pages.ProjectActions.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.Pages.ProjectActions.Index.Subscriptions.subscriptions model.indexModel
