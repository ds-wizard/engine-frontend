module Wizard.Pages.Documents.Subscriptions exposing (subscriptions)

import Wizard.Pages.Documents.Index.Subscriptions
import Wizard.Pages.Documents.Models exposing (Model)
import Wizard.Pages.Documents.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.Pages.Documents.Index.Subscriptions.subscriptions model.indexModel
