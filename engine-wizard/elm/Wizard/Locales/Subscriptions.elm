module Wizard.Locales.Subscriptions exposing (subscriptions)

import Wizard.Locales.Create.Subscriptions
import Wizard.Locales.Detail.Subscriptions
import Wizard.Locales.Import.Subscriptions
import Wizard.Locales.Index.Subscriptions
import Wizard.Locales.Models exposing (Model)
import Wizard.Locales.Msgs exposing (Msg(..))
import Wizard.Locales.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        CreateRoute ->
            Sub.map CreateMsg <|
                Wizard.Locales.Create.Subscriptions.subscriptions model.createModel

        DetailRoute _ ->
            Sub.map DetailMsg <|
                Wizard.Locales.Detail.Subscriptions.subscriptions model.detailModel

        ImportRoute _ ->
            Sub.map ImportMsg <|
                Wizard.Locales.Import.Subscriptions.subscriptions model.importModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Locales.Index.Subscriptions.subscriptions model.indexModel
