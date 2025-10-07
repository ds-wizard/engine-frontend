module Wizard.Pages.Locales.Subscriptions exposing (subscriptions)

import Wizard.Pages.Locales.Create.Subscriptions
import Wizard.Pages.Locales.Detail.Subscriptions
import Wizard.Pages.Locales.Index.Subscriptions
import Wizard.Pages.Locales.Models exposing (Model)
import Wizard.Pages.Locales.Msgs exposing (Msg(..))
import Wizard.Pages.Locales.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        CreateRoute ->
            Sub.map CreateMsg <|
                Wizard.Pages.Locales.Create.Subscriptions.subscriptions model.createModel

        DetailRoute _ ->
            Sub.map DetailMsg <|
                Wizard.Pages.Locales.Detail.Subscriptions.subscriptions model.detailModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Pages.Locales.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
