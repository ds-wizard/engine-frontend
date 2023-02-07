module Wizard.DocumentTemplates.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplates.Import.Subscriptions
import Wizard.DocumentTemplates.Index.Subscriptions
import Wizard.DocumentTemplates.Models exposing (Model)
import Wizard.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        ImportRoute _ ->
            Sub.map ImportMsg <| Wizard.DocumentTemplates.Import.Subscriptions.subscriptions model.importModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.DocumentTemplates.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
