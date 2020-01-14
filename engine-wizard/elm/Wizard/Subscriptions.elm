module Wizard.Subscriptions exposing (subscriptions)

import Wizard.Common.Menu.Subscriptions
import Wizard.KMEditor.Subscriptions
import Wizard.KnowledgeModels.Subscriptions
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                Routes.KMEditorRoute route ->
                    Wizard.KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                Routes.KnowledgeModelsRoute route ->
                    Sub.map KnowledgeModelsMsg <| Wizard.KnowledgeModels.Subscriptions.subscriptions route model.kmPackagesModel

                _ ->
                    Sub.none

        menuSubscriptions =
            Wizard.Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch [ currentViewSubscriptions, menuSubscriptions ]
