module Wizard.Subscriptions exposing (subscriptions)

import Wizard.Auth.Subscriptions
import Wizard.Comments.Subscriptions
import Wizard.Common.Driver as Driver
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
import Wizard.Ports as Ports
import Wizard.ProjectActions.Subscriptions
import Wizard.ProjectFiles.Subscriptions
import Wizard.ProjectImporters.Subscriptions
import Wizard.Projects.Subscriptions
import Wizard.Public.Subscriptions
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

                Routes.CommentsRoute _ _ ->
                    Sub.map CommentsMsg <| Wizard.Comments.Subscriptions.subscriptions model.commentsModel

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

                Routes.ProjectActionsRoute _ ->
                    Sub.map ProjectActionsMsg <| Wizard.ProjectActions.Subscriptions.subscriptions model.projectActionsModel

                Routes.ProjectFilesRoute _ ->
                    Sub.map ProjectFilesMsg <| Wizard.ProjectFiles.Subscriptions.subscriptions model.projectFilesModel

                Routes.ProjectImportersRoute _ ->
                    Sub.map ProjectImportersMsg <| Wizard.ProjectImporters.Subscriptions.subscriptions model.projectImportersModel

                Routes.ProjectsRoute route ->
                    Sub.map ProjectsMsg <| Wizard.Projects.Subscriptions.subscriptions route model.projectsModel

                Routes.PublicRoute route ->
                    Sub.map PublicMsg <| Wizard.Public.Subscriptions.subscriptions route

                Routes.UsersRoute route ->
                    Sub.map UsersMsg <| Wizard.Users.Subscriptions.subscriptions route model.users

                _ ->
                    Sub.none

        authSubscriptions =
            Wizard.Auth.Subscriptions.subscriptions model

        menuSubscriptions =
            Wizard.Common.Menu.Subscriptions.subscriptions model.menuModel

        historySubscriptions =
            Ports.historyBackCallback HistoryBackCallback

        tourSubscriptions =
            Driver.onTourDone TourDone
    in
    Sub.batch
        [ currentViewSubscriptions
        , authSubscriptions
        , menuSubscriptions
        , historySubscriptions
        , tourSubscriptions
        ]
