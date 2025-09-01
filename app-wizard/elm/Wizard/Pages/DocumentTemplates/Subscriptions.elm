module Wizard.Pages.DocumentTemplates.Subscriptions exposing (subscriptions)

import Wizard.Pages.DocumentTemplates.Detail.Subscriptions
import Wizard.Pages.DocumentTemplates.Index.Subscriptions
import Wizard.Pages.DocumentTemplates.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ ->
            Sub.map DetailMsg <| Wizard.Pages.DocumentTemplates.Detail.Subscriptions.subscriptions model.detailModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Pages.DocumentTemplates.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
