module Wizard.KnowledgeModels.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.Subscriptions
import Wizard.KnowledgeModels.Models exposing (Model)
import Wizard.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        ImportRoute _ ->
            Sub.map ImportMsg <| Wizard.KnowledgeModels.Import.Subscriptions.subscriptions model.importModel

        _ ->
            Sub.none
