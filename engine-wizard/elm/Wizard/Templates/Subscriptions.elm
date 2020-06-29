module Wizard.Templates.Subscriptions exposing (subscriptions)

import Wizard.Templates.Import.Subscriptions
import Wizard.Templates.Index.Subscriptions
import Wizard.Templates.Models exposing (Model)
import Wizard.Templates.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        ImportRoute _ ->
            Sub.map ImportMsg <| Wizard.Templates.Import.Subscriptions.subscriptions model.importModel

        IndexRoute ->
            Sub.map IndexMsg <| Wizard.Templates.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
