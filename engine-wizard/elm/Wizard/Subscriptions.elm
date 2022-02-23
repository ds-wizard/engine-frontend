module Wizard.Subscriptions exposing (subscriptions)

import Wizard.Common.Menu.Subscriptions
import Wizard.Documents.Subscriptions
import Wizard.KMEditor.Subscriptions
import Wizard.KnowledgeModels.Subscriptions
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Projects.Subscriptions
import Wizard.Routes as Routes
import Wizard.Templates.Subscriptions
import Wizard.Users.Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                Routes.DocumentsRoute route ->
                    Sub.map DocumentsMsg <| Wizard.Documents.Subscriptions.subscriptions route model.documentsModel

                Routes.KMEditorRoute route ->
                    Wizard.KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                Routes.KnowledgeModelsRoute route ->
                    Sub.map KnowledgeModelsMsg <| Wizard.KnowledgeModels.Subscriptions.subscriptions route model.kmPackagesModel

                Routes.ProjectsRoute route ->
                    Sub.map ProjectsMsg <| Wizard.Projects.Subscriptions.subscriptions route model.plansModel

                Routes.TemplatesRoute route ->
                    Sub.map TemplatesMsg <| Wizard.Templates.Subscriptions.subscriptions route model.templatesModel

                Routes.UsersRoute route ->
                    Sub.map UsersMsg <| Wizard.Users.Subscriptions.subscriptions route model.users

                _ ->
                    Sub.none

        menuSubscriptions =
            Wizard.Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch [ currentViewSubscriptions, menuSubscriptions ]
