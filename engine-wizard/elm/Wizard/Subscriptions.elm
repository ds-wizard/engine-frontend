module Wizard.Subscriptions exposing (subscriptions)

import Wizard.Auth.Subscriptions
import Wizard.Common.Menu.Subscriptions
import Wizard.Dev.Subscriptions
import Wizard.DocumentTemplateEditors.Subscriptions
import Wizard.DocumentTemplates.Subscriptions
import Wizard.Documents.Subscriptions
import Wizard.KMEditor.Subscriptions
import Wizard.KnowledgeModels.Subscriptions
import Wizard.Locales.Subscriptions
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.ProjectImporters.Subscriptions
import Wizard.Projects.Subscriptions
import Wizard.Routes as Routes
import Wizard.Tenants.Subscriptions
import Wizard.Users.Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                Routes.TenantsRoute route ->
                    Sub.map TenantsMsg <| Wizard.Tenants.Subscriptions.subscriptions route model.tenantsModel

                Routes.DevRoute route ->
                    Sub.map AdminMsg <| Wizard.Dev.Subscriptions.subscriptions route model.adminModel

                Routes.DocumentsRoute _ ->
                    Sub.map DocumentsMsg <| Wizard.Documents.Subscriptions.subscriptions model.documentsModel

                Routes.DocumentTemplateEditorsRoute route ->
                    Wizard.DocumentTemplateEditors.Subscriptions.subscriptions DocumentTemplateEditorsMsg OnTime route model.documentTemplateEditorsModel

                Routes.DocumentTemplatesRoute route ->
                    Sub.map DocumentTemplatesMsg <| Wizard.DocumentTemplates.Subscriptions.subscriptions route model.documentTemplatesModel

                Routes.KMEditorRoute route ->
                    Wizard.KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                Routes.KnowledgeModelsRoute route ->
                    Sub.map KnowledgeModelsMsg <| Wizard.KnowledgeModels.Subscriptions.subscriptions route model.kmPackagesModel

                Routes.LocalesRoute route ->
                    Sub.map LocaleMsg <| Wizard.Locales.Subscriptions.subscriptions route model.localeModel

                Routes.ProjectImportersRoute _ ->
                    Sub.map ProjectImportersMsg <| Wizard.ProjectImporters.Subscriptions.subscriptions model.projectImportersModel

                Routes.ProjectsRoute route ->
                    Sub.map ProjectsMsg <| Wizard.Projects.Subscriptions.subscriptions route model.projectsModel

                Routes.UsersRoute route ->
                    Sub.map UsersMsg <| Wizard.Users.Subscriptions.subscriptions route model.users

                _ ->
                    Sub.none

        authSubscriptions =
            Wizard.Auth.Subscriptions.subscriptions model

        menuSubscriptions =
            Wizard.Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch
        [ currentViewSubscriptions
        , authSubscriptions
        , menuSubscriptions
        ]
