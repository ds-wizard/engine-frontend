module Wizard.DocumentTemplates.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplates.Detail.Subscriptions
import Wizard.DocumentTemplates.Index.Subscriptions
import Wizard.DocumentTemplates.Models exposing (Model)
import Wizard.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ ->
            Sub.map DetailMsg <| Wizard.DocumentTemplates.Detail.Subscriptions.subscriptions model.detailModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.DocumentTemplates.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
