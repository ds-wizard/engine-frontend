module Wizard.Documents.Subscriptions exposing (subscriptions)

import Wizard.Documents.Index.Subscriptions
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map IndexMsg <| Wizard.Documents.Index.Subscriptions.subscriptions model.indexModel
